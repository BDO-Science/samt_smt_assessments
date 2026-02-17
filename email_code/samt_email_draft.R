library(tidyverse)
library(RDCOMClient)
library(knitr)
library(base64enc)
library(magick)
library(kableExtra)
library(here)

# Source the assessment pipeline (salmon_code.R + text_updates.R)
project <- here()
source(here(project, 'source_code/text_updates.R'), echo = FALSE)

#########################################################
# BUILD WEEKLY SUMMARY TABLE FROM LOSS SUMMARY TABLE
#########################################################

# loss_summary_table rows: 1 = 7-day loss, 2 = avg daily, 3 = cumulative, 4 = % threshold
weekly_email_table <- loss_summary_table %>%
  select(
    `Data Item`          = data_item,
    `Natural WR`         = dna_winter_run_chinook,
    `Hatchery WR`        = lsnfh_hatchery_cwt_winter_run_chinook,
    `Natural Steelhead`  = natural_steelhead,
    `Hatchery Steelhead` = hatchery_steelhead,
    `Spring-run`         = any_of(c("spring_run_chinook", "spring_run"))
  )

n_cols <- ncol(weekly_email_table)

weekly_html <- knitr::kable(
  weekly_email_table,
  format     = "html",
  align      = rep("l", n_cols),
  table.attr = "style='border-collapse: collapse; margin: 0; padding: 0;
                line-height: 1.2; width: 700px; font-family: Arial; font-size: 10pt;'"
) |>
  kable_styling(
    full_width   = FALSE,
    position     = "left",
    font_size    = 12,
    stripe_color = "#f9f9f9"
  ) |>
  row_spec(0, bold = TRUE,
           extra_css = "border-top: 1px solid black; border-bottom: 1px solid black;
                        white-space: normal;") |>
  column_spec(
    1:n_cols,
    width     = "120px",
    extra_css = "border-left: none; border-right: none; white-space: normal;
                 word-wrap: break-word;"
  )

#########################################################
# DOWNLOAD AND EMBED LSP IMAGES FROM CBR
#########################################################

base_url <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY', wy, '/')

embed_cbr_image <- function(url) {
  tmp <- tempfile(fileext = ".png")
  tryCatch({
    img <- magick::image_read(url) %>% magick::image_scale("900x")
    magick::image_write(img, path = tmp, format = "png")
    base64enc::dataURI(file = tmp, mime = "image/png")
  }, error = function(e) {
    warning(paste("Could not download image:", url))
    NULL
  })
}

img_wr_lsp <- embed_cbr_image(paste0(base_url, 'samt_lsp_winter.png'))
img_sh_lsp <- embed_cbr_image(paste0(base_url, 'samt_lsp_stlhd.png'))

img_tag <- function(b64, alt, caption = "") {
  if (is.null(b64)) return(paste0("<p><em>", alt, " \u2013 image unavailable</em></p>"))
  paste0(
    "<figure style='margin:0 0 12px 0;'>",
    "<img src='", b64, "' alt='", alt, "' style='width:100%; max-width:800px;'/>",
    if (nchar(caption) > 0)
      paste0("<figcaption style='font-size:9pt; font-style:italic;'>", caption, "</figcaption>")
    else "",
    "</figure>"
  )
}

#########################################################
# PRE-COMPUTE THRESHOLD AND ITL VALUES FOR EMAIL
# Natural WR: threshold (1% JPE) != ITL (0.56% / 0.36%) -> report both
# Hatchery WR: threshold = ITL = 1% JPE                 -> report once
# Natural SH:  no threshold; ITL only (5,294 / 2,319)   -> report ITL only
# Hatchery SH: threshold = ITL = 1% JPE                 -> report once
# Spring-run:  threshold = 1% JPE; ITL = 0.5% per group -> report both
#########################################################

wr_nat_threshold_val  <- prettyNum(round(jpe * wr_loss_threshold,     0), big.mark = ",")
wr_nat_itl_single_val <- prettyNum(round(jpe * itl_wr_natural_single, 0), big.mark = ",")
wr_nat_itl_3yr_val    <- prettyNum(round(jpe * itl_wr_natural_3yr,    0), big.mark = ",")

wr_hatch_threshold_val <- prettyNum(
  round(livingston_jpe * wr_hatch_loss_threshold, 0), big.mark = ","
)

#########################################################
# COMPOSE EMAIL BODY
#########################################################

email_body <- paste0(
  
  "<p style='font-family:Arial; font-size:10pt;'>Hi all,</p>",
  "<p style='font-family:Arial; font-size:10pt;'>",
  "Please see the DAT pre-FAWOG summary and assessment as of ",
  "<strong>", format(Sys.Date() - 1, "%B %d, %Y"), "</strong>. ",
  "Data are preliminary and subject to change.</p>",
  
  # ---- STATUS OVERVIEW ----
  "<h3 style='font-family:Arial; font-size:11pt; margin-bottom:4px;'>Status Overview</h3>",
  "<ul style='font-family:Arial; font-size:10pt;'>",
  "<li>", entrainment_status, "</li>",
  "<li>", salvage_status, "</li>",
  "<li>", itl_status, "</li>",
  "<li>", sr_yearling_itl_summary, "</li>",
  "<li>", wr_presence_status, "</li>",
  "<li>", sh_presence_status, "</li>",
  "</ul>",
  
  # ---- NATURAL WINTER-RUN ----
  # Threshold (1% JPE) differs from ITL (0.56% single-yr / 0.36% 3-yr) -- report both
  "<h3 style='font-family:Arial; font-size:11pt; margin-bottom:4px;'>",
  "Natural Winter-run Chinook Salmon</h3>",
  "<ul style='font-family:Arial; font-size:10pt;'>",
  "<li>JPE: <em>", prettyNum(jpe, big.mark = ","), " fish</em></li>",
  "<li>Cumulative loss: <strong>", loss_dna_wr, "</strong> (", wr_perc, " of annual loss threshold)</li>",
  "<li>Loss in past 7 days: <strong>", wr_7d, "</strong></li>",
  "<li>Annual loss threshold (1% of JPE): <strong>", wr_nat_threshold_val, " fish</strong></li>",
  "<li>Single-year ITL (0.56% of JPE): <strong>", wr_nat_itl_single_val,
  " fish</strong> &nbsp;|&nbsp; ",
  "3-year rolling ITL (0.36% of JPE): <strong>", wr_nat_itl_3yr_val,
  " fish</strong> (BiOp Table 184)</li>",
  "</ul>",
  
  # ---- HATCHERY WINTER-RUN ----
  # Threshold = ITL = 1% JPE -- report once, note equivalence
  "<h3 style='font-family:Arial; font-size:11pt; margin-bottom:4px;'>",
  "Hatchery Winter-run Chinook Salmon (Livingston Stone NFH)</h3>",
  "<ul style='font-family:Arial; font-size:10pt;'>",
  "<li>JPE: <em>", prettyNum(livingston_jpe, big.mark = ","), " fish</em></li>",
  "<li>Cumulative loss: <strong>", loss_hatch_wr, "</strong> (", wr_hatch_perc,
  " of annual loss threshold / ITL)</li>",
  "<li>Loss in past 7 days: <strong>", wr_hatch_7d, "</strong></li>",
  "<li>Annual loss threshold = Single-year ITL (1% of JPE): <strong>",
  wr_hatch_threshold_val, " fish</strong> (BiOp Table 184)</li>",
  "</ul>",
  
  # ---- NATURAL STEELHEAD ----
  # Has annual loss threshold AND ITL -- report both
  "<h3 style='font-family:Arial; font-size:11pt; margin-bottom:4px;'>",
  "Natural-origin Central Valley Steelhead</h3>",
  "<ul style='font-family:Arial; font-size:10pt;'>",
  "<li>Cumulative loss: <strong>", loss_nat_sh, "</strong> (", sh_perc,
  " of annual loss threshold)</li>",
  "<li>Loss in past 7 days: <strong>", sh_7d, "</strong></li>",
  "<li>Annual loss threshold: <strong>", prettyNum(itl_sh_natural_single, big.mark = ","),
  " fish</strong> (single-year) &nbsp;|&nbsp; <strong>",
  prettyNum(itl_sh_natural_3yr, big.mark = ","),
  " fish</strong> (3-year rolling average) (BiOp Table 184)</li>",
  "</ul>",
  
  # ---- HATCHERY STEELHEAD ----
  # Threshold = ITL = 1% JPE -- report once
  "<h3 style='font-family:Arial; font-size:11pt; margin-bottom:4px;'>",
  "Hatchery-origin Central Valley Steelhead</h3>",
  "<ul style='font-family:Arial; font-size:10pt;'>",
  "<li>Cumulative loss: <strong>", sh_clipped_loss_total, "</strong> (",
  sh_clipped_perc_threshold, " of annual loss threshold / ITL)</li>",
  "<li>Annual loss threshold = Single-year ITL (1% of JPE): <strong>",
  sh_clipped_threshold_fmt, " fish</strong> (BiOp Table 184)</li>",
  "</ul>",
  
  # ---- SPRING-RUN SURROGATES ----
  # Threshold = 1% JPE (production); ITL = 0.5% per experimental group -- report both
  "<h3 style='font-family:Arial; font-size:11pt; margin-bottom:4px;'>",
  "Spring-run Chinook Salmon Surrogates (Coleman Late-Fall)</h3>",
  "<ul style='font-family:Arial; font-size:10pt;'>",
  "<li>Total releases: <strong>", total_sr_released_fmt, "</strong> fish &nbsp;|&nbsp; ",
  "JPE: <strong>", total_sr_jpe_fmt, "</strong></li>",
  "<li>Cumulative confirmed loss: <strong>", sr_loss_total_fmt, "</strong> (",
  sr_loss_perc, " of annual loss threshold)</li>",
  "<li>Annual loss threshold (1% of JPE): <strong>", sr_threshold_fmt, " fish</strong></li>",
  "<li>ITL per experimental release group (0.5% of each group, BiOp Table 184): ",
  sr_itl_text, "</li>",
  "</ul>",
  
  # ---- RISK EVALUATION ----
  "<h3 style='font-family:Arial; font-size:11pt; margin-bottom:4px; margin-top:12px;'>",
  "Risk Evaluation</h3>",
  "<ol style='font-family:Arial; font-size:10pt;'>",
  "<li><strong>Natural and hatchery winter-run Chinook:</strong> ", risk_q1, "</li>",
  "<li><strong>Spring-run Chinook surrogates:</strong> ", risk_q2, "</li>",
  "<li><strong>Natural and hatchery steelhead:</strong> ", risk_q3, "</li>",
  "</ol>",
  
  # ---- LSP FIGURES ----
  "<h3 style='font-family:Arial; font-size:11pt; margin-bottom:4px;'>Loss Predictor Figures</h3>",
  img_tag(img_wr_lsp, "Winter-run Loss Predictor",
          "Figure 1. Estimates of winter-run Chinook loss generated by the Loss and Salvage Predictor tool."),
  img_tag(img_sh_lsp, "Steelhead Loss Predictor",
          "Figure 2. Estimates of steelhead loss generated by the Loss and Salvage Predictor tool."),
  
  "<p style='font-family:Arial; font-size:9pt;'>",
  "For more detailed data on salmonid conditions in the Delta see ",
  "<a href='https://www.cbr.washington.edu/sacramento/workgroups/salmon_monitoring.html'>SacPAS</a>.",
  "</p>",
  
  "<p style='font-family:Arial; font-size:10pt;'>Best regards,<br>SaMT Team</p>"
)

#########################################################
# CREATE AND SAVE OUTLOOK DRAFT VIA RDCOMClient
#########################################################

outlook_app <- COMCreate("Outlook.Application")
email       <- outlook_app$CreateItem(0)  # 0 = olMailItem

email[["To"]] <- paste(
  "geasterbrook@usbr.gov",
  "jvogel@usbr.gov",
  "JAIsrael@usbr.gov",
  "avaisvil@usbr.gov",
  "RField@usbr.gov",
  "lejohnson@usbr.gov",
  "ashamilton@usbr.gov",
  "tyang@usbr.gov",
  "dmmooney@usbr.gov",
  "ebuttermore@usbr.gov",
  "jfenolio@usbr.gov",
  sep = "; "
)

email[["Subject"]]  <- paste0("DAT pre-FAWOG summary and assessment \u2013 ", format(Sys.Date(), "%B %d, %Y"))
email[["HTMLBody"]] <- email_body
email$Save()

message("Draft saved to Outlook Drafts folder.")