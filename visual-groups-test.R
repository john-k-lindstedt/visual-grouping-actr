require(ggplot2)
require(png)
require(grid)

setwd("~/Dropbox/Projects/2017 Ballot Models/code/visual-grouping")

vg <- read.delim("./data/test.tsv", sep="\t")
vbimg <- rasterGrob(readPNG("./assets/vbimg.png"), interpolate=T)

vg$left = vg$screen.x - (vg$width / 2.0)
vg$right = vg$screen.x + (vg$width / 2.0)
vg$bottom = vg$screen.y - (vg$height / 2.0)
vg$top = vg$screen.y + (vg$height / 2.0)

vgbounds = ddply(vg,.(group),summarize,xmin = min(screen.x - (width / 2)),xmax = max(screen.x + (width / 2)), ymin = min(screen.y - (height / 2)),ymax = max(screen.y + (height / 2)))

ggplot(vg, aes(color = group))+
  annotation_custom(vbimg, xmin=0, xmax=1024, ymin=0, ymax=-768)+
  geom_rect(xmin = 0, xmax = 1024, ymin = 0, ymax = -768, fill = "white", color = "white", alpha = .05)+
  geom_rect(data = vgbounds, aes(xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax), size = 1, alpha = .5, fill = NA)+
  geom_rect(aes(xmin = left,xmax = right,
                ymin = bottom,ymax = top,
                fill = group), 
            size = 1, alpha = .3, color = NA)+
  geom_point(aes(x=screen.x,y=screen.y),size = 3)+
  scale_y_reverse()+
  coord_fixed(xlim = c(0,1024),ylim=c(0,768))+
  scale_color_brewer(palette="Paired")+
  scale_fill_brewer(palette="Paired")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())




#with glom radius boxes
ggplot(vg, aes(color = group))+
  annotation_custom(vbimg, xmin=0, xmax=1024, ymin=0, ymax=-768)+
  geom_rect(xmin = 0, xmax = 1024, ymin = 0, ymax = -768, fill = "white", color = "white", alpha = .05)+
  #geom_rect(data = vgbounds, aes(xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax), size = 1, alpha = .7, fill = NA)+
  geom_rect(aes(xmin = screen.x - (width / 2),xmax = screen.x + (width / 2),
                ymin = screen.y - (height / 2),ymax = screen.y + (height / 2),
                fill = group), 
            size = 1, alpha = .5, color = NA)+
  geom_point(aes(x=screen.x,y=screen.y),size = 3)+
  geom_segment(aes(x = left, xend = right,
                   y=top + radius,yend=top + radius), 
               alpha = 1,size=.5)+
  geom_segment(aes(x = left, xend = right,
                   y=bottom - radius,yend=bottom - radius), 
               alpha = 1,size=.5)+
  geom_segment(aes(x = left - radius,xend = left - radius,
                   y=top,yend=bottom), 
               alpha = 1,size=.5)+
  geom_segment(aes(x = right + radius,xend = right + radius,
                   y=top, yend=bottom), 
               alpha = 1,size=.5)+
  geom_curve(aes(  x = left - radius,xend = left,
                   y=top,yend=top+radius), 
               alpha = 1,size=.5,curvature = .5)+
  geom_curve(aes(  x = right,xend = right + radius,
                   y=top+radius,yend=top), 
               alpha = 1,size=.5,curvature = .5)+
  geom_curve(aes(  x = right + radius,xend = right,
                   y=bottom,yend=bottom-radius),
               alpha = 1,size=.5,curvature = .5)+
  geom_curve(aes(  x = left,xend = left-radius,
                   y=bottom-radius,yend=bottom),
               alpha = 1,size=.5,curvature = .5)+
  scale_y_reverse()+
  coord_fixed(xlim = c(0,1024),ylim=c(0,768))+
  scale_color_brewer(palette="Paired")+
  scale_fill_brewer(palette="Paired")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("plots/glom_rects.pdf", width = 11, height = 8.5)


#with simple point radius
ggplot(vg, aes(color = group))+
  annotation_custom(vbimg, xmin=0, xmax=1024, ymin=0, ymax=-768)+
  geom_rect(xmin = 0, xmax = 1024, ymin = 0, ymax = -768, fill = "white", color = "white", alpha = .05)+
  #geom_rect(data = vgbounds, aes(xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax), size = 1, alpha = .7, fill = NA)+
  geom_rect(aes(xmin = screen.x - (width / 2),xmax = screen.x + (width / 2),
                ymin = screen.y - (height / 2),ymax = screen.y + (height / 2),
                fill = group), 
            size = 1, alpha = .3, color = NA)+
  geom_point(aes(x=screen.x,y=screen.y),size = 3)+
  geom_curve(aes(x = screen.x - radius, xend = screen.x + radius, y=screen.y, yend=screen.y),alpha = 1,size=.5,curvature = 1)+
  geom_curve(aes(x = screen.x - radius, xend = screen.x + radius, y=screen.y, yend=screen.y),alpha = 1,size=.5,curvature = -1)+
  scale_y_reverse()+
  coord_fixed(xlim = c(0,1024),ylim=c(0,768))+
  scale_color_brewer(palette="Paired")+
  scale_fill_brewer(palette="Paired")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())



ggsave("glom_method.pdf", width = 11, height = 8.5)










ggplot(clusts, aes(color = name))+
  annotation_custom(vbimg, xmin=0, xmax=1024, ymin=0, ymax=-768)+
  geom_rect(xmin = 0, xmax = 1024, ymin = 0, ymax = -768, fill = "white", color = "white", alpha = .05)+
  geom_rect(aes(xmin = x.min,xmax = x.max,ymin = y.min,ymax = y.max), size = 1, alpha = .7, fill = NA)+
  #geom_rect(aes(xmin = center.x - sd.x, xmax = center.x + sd.x, 
  #              ymin = center.y - sd.y, ymax = center.y + sd.y, 
  #              fill = name), color = NA, alpha = .2)+
  #geom_rect(aes(xmin = center.x - (sd.x*2), xmax = center.x + (sd.x*2), 
  #              ymin = center.y - (sd.y*2), ymax = center.y + (sd.y*2), 
  #              fill = name), color = NA, alpha = .1)+
  #geom_rect(aes(xmin = center.x - (sd.x*3), xmax = center.x + (sd.x*3), 
  #              ymin = center.y - (sd.y*3), ymax = center.y + (sd.y*3), 
  #              fill = name), color = NA, alpha = .05)+
  geom_point(aes(x = center.x, y = center.y), shape = 3, size = 4, alpha = 1)+
  geom_point(data = obsvs, aes(x=x,y=y,color=name), size = 3)+
  xlab("x")+
  ylab("y")+
  scale_y_reverse()+
  coord_fixed(xlim = c(0,1024),ylim=c(0,768))+
  scale_color_brewer(palette="Paired")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("kmeans_method.pdf", width = 11, height = 8.5)





glom <- read_delim("~/Dropbox/Projects/2017 Ballot Models/code/glom_method/data.tsv", "\t")
glomlisp <- read_delim("~/Dropbox/Projects/2017 Ballot Models/code/glom_method/glom-lisp-test.tsv", "\t")

glom$group = as.factor(glom$group)
glomlisp$group = as.factor(glomlisp$group)

require(plyr)
glombox = ddply(glom,.(group),summarize,xmin = min(x),xmax = max(x), ymin = min(y),ymax = max(y))
glomlispbox = ddply(glomlisp,.(group),summarize,xmin = min(x),xmax = max(x), ymin = min(y),ymax = max(y))


ggplot(glomlisp, aes(color = group))+
  annotation_custom(vbimg, xmin=0, xmax=1024, ymin=0, ymax=-768)+
  geom_rect(xmin = 0, xmax = 1024, ymin = 0, ymax = -768, fill = "white", color = "white", alpha = .01)+
  geom_rect(data = glomlispbox, aes(xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax), size = 1, alpha = .7, fill = NA)+
  geom_point(aes(x=x,y=y),size = 3)+
  #geom_segment(aes(x = x-r,xend = x+r,y=y,yend=y),alpha = .3,size=2)+
  #geom_segment(aes(x = x,xend = x,y=y-r,yend=y+r),alpha = .3,size=2)+
  #geom_segment(aes(x = x-(r/sqrt(2)),xend = x+(r/sqrt(2)),y=y-(r/sqrt(2)),yend=y+(r/sqrt(2))),alpha = .3,size=2)+
  #geom_segment(aes(x = x-(r/sqrt(2)),xend = x+(r/sqrt(2)),y=y+(r/sqrt(2)),yend=y-(r/sqrt(2))),alpha = .3,size=2)+
  #geom_curve(aes(x = x-r,xend = x+r,y=y,yend=y),alpha = 1,size=.5,curvature = 1)+
  #geom_curve(aes(x = x-r,xend = x+r,y=y,yend=y),alpha = 1,size=.5,curvature = -1)+
  #geom_circle(aes(x0=x,y0=-y,r=r,fill=group),alpha = .3)+
  scale_y_reverse()+
  coord_fixed(xlim = c(0,1024),ylim=c(0,768))+
  scale_color_brewer(palette="Paired")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("glom_method.pdf", width = 11, height = 8.5)



ggplot(glomlisp, aes(x = x, y = y, color = group))+
  annotation_custom(vbimg, xmin=0, xmax=1024, ymin=0, ymax=-768)+
  geom_rect(xmin = 0, xmax = 1024, ymin = 0, ymax = -768, fill = "white", color = "white", alpha = .01)+
  geom_point(size = 3)+
  #geom_segment(aes(x = x-r,xend = x+r,y=y,yend=y),alpha = .3,size=2)+
  #geom_segment(aes(x = x,xend = x,y=y-r,yend=y+r),alpha = .3,size=2)+
  #geom_segment(aes(x = x-(r/sqrt(2)),xend = x+(r/sqrt(2)),y=y-(r/sqrt(2)),yend=y+(r/sqrt(2))),alpha = .3,size=2)+
  #geom_segment(aes(x = x-(r/sqrt(2)),xend = x+(r/sqrt(2)),y=y+(r/sqrt(2)),yend=y-(r/sqrt(2))),alpha = .3,size=2)+
  geom_curve(aes(x = x-75,xend = x+75,y=y,yend=y),alpha = 1,size=.5,curvature = 1)+
  geom_curve(aes(x = x-75,xend = x+75,y=y,yend=y),alpha = 1,size=.5,curvature = -1)+
  #geom_circle(aes(x0=x,y0=-y,r=r,fill=group),alpha = .3)+
  scale_y_reverse()+
  coord_fixed(xlim = c(0,1024),ylim=c(0,768))+
  scale_color_brewer(palette="Paired")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("glom_method_radius.pdf", width = 11, height = 8.5)
