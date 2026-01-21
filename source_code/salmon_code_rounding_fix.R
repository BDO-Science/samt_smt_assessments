# salmon_code_rounding_fix.R
# Source this AFTER salmon_code.R to round all fish counts to whole numbers

# Round all loss values to whole numbers
if(exists("loss_dna_wr")) loss_dna_wr <- round(as.numeric(loss_dna_wr), 0)
if(exists("loss_lad_wr")) loss_lad_wr <- round(as.numeric(loss_lad_wr), 0)
if(exists("loss_hatch_wr")) loss_hatch_wr <- round(as.numeric(loss_hatch_wr), 0)
if(exists("loss_nat_sh")) loss_nat_sh <- round(as.numeric(loss_nat_sh), 0)
if(exists("loss_hatch_sh")) loss_hatch_sh <- round(as.numeric(loss_hatch_sh), 0)
if(exists("total_loss")) total_loss <- round(as.numeric(total_loss), 0)

# Round cumulative loss values shown in document
if(exists("wr_loss")) wr_loss <- round(as.numeric(wr_loss), 0)
if(exists("wr_hatch_loss")) wr_hatch_loss <- round(as.numeric(wr_hatch_loss), 0)
if(exists("sh_loss")) sh_loss <- round(as.numeric(sh_loss), 0)

# Round 7-day loss values
if(exists("wr_7d")) wr_7d <- round(as.numeric(wr_7d), 0)
if(exists("wr_hatch_7d")) wr_hatch_7d <- round(as.numeric(wr_hatch_7d), 0)
if(exists("sh_7d")) sh_7d <- round(as.numeric(sh_7d), 0)

# Round hatchery loss totals
if(exists("sh_clipped_loss_total")) sh_clipped_loss_total <- round(as.numeric(sh_clipped_loss_total), 0)

# Round spring-run values (check if they exist first)
if(exists("total_sr_released")) {
  total_sr_released <- round(total_sr_released, 0)
  total_sr_released_fmt <- prettyNum(total_sr_released, big.mark = ",")
}

if(exists("sr_loss_total")) {
  sr_loss_total <- round(sr_loss_total, 0)
  sr_loss_total_fmt <- prettyNum(sr_loss_total, big.mark = ",")
}

if(exists("coleman_total")) {
  coleman_total <- round(coleman_total, 0)
  coleman_total_fmt <- prettyNum(coleman_total, big.mark = ",")
}

if(exists("coleman_loss")) {
  coleman_loss <- round(coleman_loss, 0)
  coleman_loss_fmt <- prettyNum(coleman_loss, big.mark = ",")
}

if(exists("total_sr_jpe")) {
  total_sr_jpe <- round(total_sr_jpe, 0)
  total_sr_jpe_fmt <- prettyNum(total_sr_jpe, big.mark = ",")
}

if(exists("coleman_jpe")) {
  coleman_jpe <- round(coleman_jpe, 0)
  coleman_jpe_fmt <- prettyNum(coleman_jpe, big.mark = ",")
}

# Round JPE values
if(exists("jpe")) jpe <- round(jpe, 0)
if(exists("livingston_jpe")) livingston_jpe <- round(livingston_jpe, 0)

# Round threshold values
if(exists("sr_threshold_val")) {
  sr_threshold_val <- round(sr_threshold_val, 0)
  sr_threshold_fmt <- prettyNum(sr_threshold_val, big.mark = ",")
}

# Round passage estimates (keep at 2 decimals for millions)
# wr_passage and sr_passage are already in millions with 2 decimals - leave as is

print("All fish counts rounded to whole numbers")