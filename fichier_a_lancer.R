source("main.R")  # load the var data
source("visualisation1.R")  # plot_tree_distribution(), plot_types_feuillage(), plot_boxplot_tronc_diam(), plot_types_feuillage(), create_histogram(), plot_arbre_quartier(),
source("visualisation2.R")  # plot_carte(), plot_quartiers(),
source("test_leaflet.R")  # load the var map, just type map in the console to display it
source("etude_correlation.R")  # plot_correlations(), planter_arbre(), find_explicative_var_for_age(), find_trees_to_cut(), perform_chi2_tests()
source("export.R")  # export_csv()


# call a function loaded from the files above
# call plot_quartiers() at the very end because it creates a layout of 2 subplots and it remains for the other plots
