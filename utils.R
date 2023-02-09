calculate_4d_matrix_from_Spectra <- function(Spectra){
    if (all(c("mz", "intensity", "retention_time", "inv_ion_mobility") %in% peaksVariables(Spectra))){
        sp <- do.call(rbind,
                      peaksData(Spectra,
                                c("mz", "intensity", "retention_time", "inv_ion_mobility")))
    } else {
        ## Failsafe-ish approach, but slower
        pd <- peaksData(Spectra, c("mz", "intensity"))
        sp <- do.call(rbind,
                      lapply(seq_along(pd), 
                             FUN = function(i, pd, rt, im){
                                 cbind(pd[[i]],
                                       rep(rt[i,1], nrow(pd[[i]])),
                                       rep(im[i,1], nrow(pd[[i]])))
                             },
                             pd = pd,
                             rt = spectraData(Spectra, c("rtime")),
                             im = spectraData(Spectra, c("inv_ion_mobility"))
                             )
                      )
        colnames(sp) <- c("mz", "intensity", "retention_time", "inv_ion_mobility")
    }
    sp
}

create_marginal_plot <- function(data){
    main_rtmz <- ggplot(data) + geom_point(aes(x=retention_time, y=mz, color = log10(intensity))) + theme_minimal() + theme(legend.position = "none") 
    main_immz <- ggplot(data) + geom_point(aes(x=inv_ion_mobility, y=mz, color = log10(intensity)))  + theme_minimal() +theme(legend.position = "none") 
    
    
    if ("dataOrigin" %in% colnames(data)) {
        margin_rt_plot <- calculate_margin_plot_from_data(data, "retention_time", bySample = TRUE)
        margin_im_plot <- calculate_margin_plot_from_data(data, "inv_ion_mobility", bySample = TRUE)
        margin_mz_plot <- calculate_margin_plot_from_data(data, "mz", bySample = TRUE) + coord_flip()
    } else {
        margin_rt_plot <- calculate_margin_plot_from_data(data, "retention_time")
        margin_im_plot <- calculate_margin_plot_from_data(data, "inv_ion_mobility")
        margin_mz_plot <- calculate_margin_plot_from_data(data, "mz") + coord_flip()
    }

    plot <- cowplot::plot_grid(margin_im_plot, margin_rt_plot, NULL,
                               main_immz,      main_rtmz,      margin_mz_plot,
                               rel_heights = c(0.3, 0.7),
                               rel_widths = c(0.4, 0.4, 0.2),
                               nrow = 2, ncol = 3)
    return(plot)
}

calculate_margin_plot_from_data <- function(data, groupVar, bySample = FALSE, sampleVar = "dataOrigin"){
    if(bySample) groupVar <- c(groupVar, sampleVar)
    margin_data <- do.call(cbind, summarize(group_by(data, !!sym(groupVar)), intensity = sum(intensity)))
    if(bySample){
        return(ggplot(as.data.frame(margin_data)) +
            geom_line(aes(x=groupVar, y=intensity, color = .data[[sampleVar]], group = .data[[sampleVar]])) +
            theme_minimal())
    } else {
        return(ggplot(as.data.frame(margin_data)) +
            geom_line(aes(x=.data[[groupVar]], y=intensity)) +
            theme_minimal())
    }
}
