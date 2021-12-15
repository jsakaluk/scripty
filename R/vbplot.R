#' A Function For Performing and Visualizing a van Bork et al. (2019) Test of Unidimensional Factor Models vs. Simple Network Models
#'
#' This produces an APA-formatted ggplot of the distributions of simulation results under
#' the UFM and SNM, with a vertical line corresponding to the observed data
#' @param df input data frame consisting of variables in the measurement model
#' @param n  input number of simulated samples to use if specifying latent or network model. If comparing ufm and snm, the same
#' n should be specified.
#' @param seed  input number to set random seed for simulations. Defaults to 705.
#' @export
#' @examples
#' plot <- vbplot(df, n = 10000, seed = 123)

vbplot <- function(df, n, seed=123){
  obs <- CorPcorProp(df, type = "obs")
  ufm <- CorPcorProp(df, type = "ufm", n = n, seed = seed)
  snm <- CorPcorProp(df, type = "snm", n = n, seed = seed)

    apatheme <-
      theme_bw()+                                      #apply ggplot2() black and white theme
      theme(panel.grid.major = element_blank(),        #eliminate major grid lines
            panel.grid.minor = element_blank(),        #eliminate minor grid lines
            panel.background = element_blank(),        #eliminate the square panel background
            panel.border = element_blank(),            #eliminate the square panel border
            legend.title=element_blank(),              #eliminate lengend title
            legend.position= "right",                  #position legend to the right of the plot
            axis.line.x = element_line(color="black"), #include a black border along the x-axis
            axis.line.y = element_line(color="black")) #include a black border along the y-axis

    plot <- ggplot()+
      xlim(-.02,1)+
      geom_histogram(data = ufm, aes(x = value, fill = "UFM"), alpha = 0.6,position = 'identity')+
      geom_histogram(data = snm, aes(x = value, fill = "SNM"), alpha = 0.6,position = 'identity')+
      geom_vline(aes(xintercept = obs, linetype = "Data"))+
      scale_fill_grey(start = 0.2, end = 0.8) +
      scale_linetype_manual(values = ("dashed"))+
      labs(x = "Proportion of Partial Correlations", y= "Number of Simulated Samples", fill="",title=" ")+
      apatheme
    plot

    return(plot)
}
