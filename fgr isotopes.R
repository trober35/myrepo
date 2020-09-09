library(simmr)
system.file("extdata", "geese_data.xls", package = "simmr")
library(readxl)
path = system.file("extdata", "geese_data.xls", package = "simmr")
geese_data = lapply(excel_sheets(path), read_excel, path = path)



system(paste('open',path))
targets = geese_data[[1]]
sources = geese_data[[2]]
TEFs = geese_data[[3]]
concdep = geese_data[[4]]



geese_simmr = simmr_load(mixtures = as.matrix(targets[, 1:2]),
                         source_names = sources$Sources,
                         source_means = sources[,2:3],
                         source_sds = sources[,4:5],
                         correction_means = TEFs[,2:3],
                         correction_sds = TEFs[,4:5],
                         concentration_means = concdep[,2:3],
                         group = as.factor(paste('Day', targets$Time)))

plot(geese_simmr, group = 1:8)


geese_simmr_out = simmr_mcmc(geese_simmr)
summary(geese_simmr_out, type = 'diagnostics',
        group = 1)


posterior_predictive(geese_simmr_out, group = 5)
prior_viz(geese_simmr_out)
plot(geese_simmr_out, type = 'histogram')





# Start of fish data

LAT<-simmr_data_2020
prey<-prey_species_simmr
TEFs<-TEFs_simmr

LAT_simmr = simmr_load(mixtures = as.matrix(LAT[, 1:3]),
                         source_names = TEFs_simmr$Sources,
                         source_means = TEFs_simmr[,2:3],
                         source_sds = TEFs_simmr[,4:5],
                         group = NULL)
plot(LAT_simmr)
LAT_simmr_out = simmr_mcmc(LAT_simmr)
summary(LAT_simmr_out, type = 'diagnostics',
        group = 1)


posterior_predictive(LAT_simmr_out)
prior_viz(LAT_simmr_out)
plot(LAT_simmr_out, type = 'histogram')

#simmr with C and S

LAT_simmrS = simmr_load(mixtures = as.matrix(LAT[, 1:3]),
                       source_names = TEFs$Sources,
                       source_means = TEFs[,2:4],
                       source_sds = TEFs[,5:7],
                       group = NULL)
plot(LAT_simmrS)
LAT_simmr_out = simmr_mcmc(LAT_simmrS)
summary(LAT_simmr_out, type = 'diagnostics',
        group = 1)


posterior_predictive(LAT_simmr_out)
prior_viz(LAT_simmr_out)
plot(LAT_simmr_out, type = 'histogram')
