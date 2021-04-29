# works:
# theme
# automatic application of the theme
# continuous and discrete colour and fill scales
# automatic application of these scales
# logo placement (though for now with locally sources file)

# left to do
# package the whole thing nicely
# review the colours
#   the continuous scale gets muddled up in the middle of the range
#   will is yet to confirm final codes in the graphics charter
# add additional scales if needed
#   divergent
#   good-bad
#   ordered factor scale
# see if we can restructure the functions for less repetition

# -- links, references

# https://colorbrewer2.org/#type=qualitative&scheme=Set1&n=9
# https://coolors.co/292562-bf2229-8dc63f-681257-ffc425-1d5e34-ec7931-24aae1-763a26-8f8f8f
# https://github.com/bbc/bbplot/blob/master/R/bbc_style.R
# https://projects.susielu.com/viz-palette?colors=[%22#292562%22,%22#bf2229%22,%22#8dc63f%22,%22#681257%22,%22#ffc425%22,%22#1d5e34%22,%22#ec7931%22,%22#24aae1%22,%22#763a26%22,%22#8f8f8f%22]&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22normal%22
# https://unhcr-web.github.io/unhcRstyle/docs/


# -- Download and install the Roboto font linked in the graphics charter --

# -- run below once --

# install.packages("extrafont")
# extrafont::font_import()
# extrafont::loadfonts(device = "win")
# extrafont::fonts()


# library(ggplot2)

# WSCBlue = "#292562"
#
# WSC5Colours = c("#C4D5D5", "#A46F92", "#884269", "#77113A", "#38272F") #not actually correct colours, will will fix the graphic charter
# WSCColourRamp = colorRampPalette(WSC5Colours)
# WSCQualColours = c("#292562", "#bf2229", "#8dc63f", "#681257","#ffc425", "#1d5e34", "#ec7931", "#24aae1", "#763a26", "#8f8f8f") # i just quickly picked what seems to work okay, should be probably revised. good to have more contrast between bold colours in the beginning, and the position matters, as values later are plucked from this palette to create divergent scales.
# names(WSCQualColours) = c(NA, "Bad", "Good", NA, "Divergent1", NA, NA, "Divergent2", NA, NA)
# # # -- some draft for divergent colour scale, not really tested. would be good for them to come from the main qual colours, for consistency
# WSCDivergentColours = colorRampPalette(c(WSCQualColours["Divergent1"],"#F5F5F5",WSCQualColours["Divergent2"]))
# WSCGoodBadColours = colorRampPalette(c(WSCQualColours["Good"],"#F5F5F5",WSCQualColours["Bad"]))

theme_WSC <- function (base_size = 11,
                       base_family = "",
                       base_line_size = base_size/22,
                       base_rect_size = base_size/22) {

  WSCBlue = "#292562"
  WSCBlueLight = "#29256233"
  WSCBlueLighter = "#2925620B"

  theme_bw(
    # base_size = base_size,
    # base_family = base_family,
    # base_line_size = base_line_size,
    # base_rect_size = base_rect_size
  ) %+replace%

    theme(text = element_text(color = WSCBlue, family = "Roboto Slab"),
          panel.border = element_rect(fill = NA, colour = WSCBlueLight),
          panel.grid.major = element_line(color = WSCBlueLight),
          # panel.grid.major.x = element_blank(),
          # panel.grid.major.y = element_line(color = WSCBlueLight),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = WSCBlue),
          axis.text = element_text(color = WSCBlue, size = rel(.8)),
          # legend.position = "bottom",
          # plot.caption.position = "plot"
    )
}

theme_set(theme_WSC)

WSCPlaceLogo <- function () {
  WSClogoPNG = png::readPNG(here::here("WSC_Logo_Primary.png"))
  annotation_custom(grid::gTree(children=grid::gList(grid::rasterGrob(image = WSClogoPNG, x=1, y=1, just=1, vjust=1, width = grid::unit(1.66, "cm"), height = grid::unit(1, "cm"), interpolate = FALSE))))
}

# ---- setting default colour scales - though not all cases are covered. for example ordered factors are rendered with viridis rather than default continuous scale

scale_colour_continuous <- function(...) {
    WSC5Colours = c("#C4D5D5", "#A46F92", "#884269", "#77113A", "#38272F") #not actually correct colours, will will fix the graphic charter
  colorRampPalette(WSC5Colours)
  scale_color_gradientn(colours =  c("#C4D5D5", "#A46F92", "#884269", "#77113A", "#38272F"))
}

# ggplot(head(diamonds,1000), aes(x=carat, y=price, color=price)) + geom_point()
# scale_color_continuous  <- function(...) {scale_colour_continuous(...)} # i don't think this is actually needed

scale_fill_continuous  <- function(...) {
  WSC5Colours = c("#C4D5D5", "#A46F92", "#884269", "#77113A", "#38272F") #not actually correct colours, will will fix the graphic charter
  colorRampPalette(WSC5Colours)
  scale_fill_gradientn(colours =  c("#C4D5D5", "#A46F92", "#884269", "#77113A", "#38272F"))
}

# ggplot(mpg[c(1,3,5,7),], aes(x=displ, y=hwy, fill=displ)) + geom_bar(stat="identity")

scale_colour_discrete <- function (...) {
  WSCQualColours = c("#292562", "#bf2229", "#8dc63f", "#681257", "#ffc425", "#1d5e34", "#ec7931", "#24aae1", "#763a26", "#8f8f8f") # i just quickly picked what seems to work okay, should be probably revised. good to have more contrast between bold colours in the beginning, and the position matters, as values later are plucked from this palette to create divergent scales.
  scale_colour_manual(values = WSCQualColours)

}

# ggplot(mpg[1:100,], aes(x=trans, y=hwy, colour=manufacturer)) + geom_point()

scale_fill_discrete <- function (...) {
  WSCQualColours = c("#292562", "#bf2229", "#8dc63f", "#681257", "#ffc425", "#1d5e34", "#ec7931", "#24aae1", "#763a26", "#8f8f8f") # i just quickly picked what seems to work okay, should be probably revised. good to have more contrast between bold colours in the beginning, and the position matters, as values later are plucked from this palette to create divergent scales.
  scale_fill_manual(values = WSCQualColours)
}

# ggplot(mpg[c(1,3,5,7),], aes(x=displ, y=hwy, fill=trans)) + geom_bar(stat="identity")


# ---- testing

# ggplot(iris, aes(color = Petal.Length, y = Sepal.Length, x = Sepal.Width)) +
#   geom_point() +
#   labs(title = "Lavage de mains",
#        subtitle = paste(
#          strwrap(
#            x = "On the left are plots done using default ggplot2 settings, on the right are plots with custom WSC theme applied. The top plots use a continuous colour scale, and the 5-colour WSC scale is interpolated to create a colour ramp.",
#            width = 96),
#          collapse = "\n"),
#        caption = paste(
#          strwrap(
#            x = "This plot data and captions are completely made up. The WSC global team implemented two pilots of the whole methodology in Burkina Faso (ISO alpha-3 code BFA) and Afghanistan (ISO alpha-3 code AFG). National Cluster Coordinators (NCC) were contacted to access their calculation files in order to have more details. As the time allocated to this benchmark was quite limited and contacting all NCCs to acquire files is time consumming, this could not be reproduced in other countries, except Iraq, Mali, and Niger where previous support offered by REACH allowed to retrieve the files easily.",
#            width = 115),
#          collapse = "\n"),
#        x = "Frequency",
#        y = "Duration",
#        color = "Intensity") ->
# plot1
#
# ggplot(mpg) + aes(x = displ, fill = class) + geom_density(position = "stack", alpha=.7) +
#   labs(title = "Sanitation access by type of vehicle owned",
#        subtitle = paste(
#          strwrap(
#            x = "I just quickly picked a few colours that seem to work okay, though they should be probably revised. They have decent separation, and work not worse that most palettes for colour blindness. Position of colours in the main vector matters, as values later are plucked from it to create the 2 divergent scales.",
#            width = 96),
#          collapse = "\n"),
#        x = "Frequency",
#        y = "Duration",
#        fill = "Vehicle") ->
#   plot2
#
# gridExtra::grid.arrange(plot1,
#                         plot1 + theme_WSC() + scale_color_gradientn(colours =  WSC5Colours) + WSCPlaceLogo(),
#                         plot2,
#                         plot2 + theme_WSC() + scale_fill_manual(values = WSCQualColours) + WSCPlaceLogo(),
#                         # plot3 +
#                         #   theme(legend.position = "none"),
#                         # plot3 + theme_WSC() + scale_fill_manual(values = WSCQualColours) +
#                         #   theme(legend.position = "none"),
#                         nrow = 2 )


