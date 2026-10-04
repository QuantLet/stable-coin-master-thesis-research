
# Run the complete FRM Stable Coins analysis pipeline
source("FRM_SC_config.R")
source("FRM_SC_load_data.R")
source("FRM_SC_estimation_varying.R")
source("FRM_SC_history_outputs.R")
source("FRM_SC_frm_plot_stable.R")
source("FRM_SC_build_fixed_from_csv.R")
source("FRM_SC_centrality.R")
source("FRM_SC_crypto_compare.R")
source("FRM_SC_hhi.R")
source("FRM_SC_portfolio_dynamic.R")
source("FRM_SC_portfolio_LTEC.R")
source("FRM_SC_hhi_vs_frm.R")
source("FRM_SC_centrality_indicators.R")
source("FRM_SC_network_gif.R")
message("✅ Complete FRM analysis pipeline finished!")

