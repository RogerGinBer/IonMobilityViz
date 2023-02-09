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
    
    
    margin_rt <- do.call(cbind, summarize(group_by(data, retention_time), intensity = sum(intensity)))  
    margin_rt_plot <- ggplot(as.data.frame(margin_rt)) + geom_line(aes(x=retention_time, y=intensity)) + theme_minimal()
    
    margin_im <- do.call(cbind, summarize(group_by(data, inv_ion_mobility), intensity = sum(intensity))) 
    margin_im_plot <- ggplot(as.data.frame(margin_im)) + geom_line(aes(x=inv_ion_mobility, y=intensity)) + theme_minimal()
    
    margin_mz <- do.call(cbind, summarize(group_by(data, mz), intensity = sum(intensity))) 
    margin_mz_plot <- ggplot(as.data.frame(margin_mz)) + geom_point(aes(x=mz, y=intensity)) + coord_flip() + theme_minimal()
    
    plot <- cowplot::plot_grid(margin_im_plot, margin_rt_plot, NULL,
                               main_immz,      main_rtmz,      margin_mz_plot,
                               rel_heights = c(0.3, 0.7),
                               rel_widths = c(0.4, 0.4, 0.2),
                               nrow = 2, ncol = 3)
    return(plot)
}
