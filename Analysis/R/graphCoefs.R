library(tidyverse)
library(magrittr)
library(tabulizer) # from ropensci/tabulizer on github
library(purrr)
library(xtable)
library(scales)
library(ggrepel)
library(ggpubr)
library(ggthemes)
library(facetscales) # from zeehio/facetscales on github
#library(unixtools) # from RForge.net
#unixtools::set.tempdir("~/R/tmp")


#------------------------------------------------------------------------------
# Add custom facet wrap function for later use 
# (source: https://fishandwhistle.net/post/2018/modifying-facet-scales-in-ggplot2/)
#------------------------------------------------------------------------------
scale_override <- function(which, scale) {
      if(!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) {
              stop("which must be an integer of length 1")
  }
  
  if(is.null(scale$aesthetics) || !any(c("x", "y") %in% scale$aesthetics)) {
          stop("scale must be an x or y position scale")
    }
    
    structure(list(which = which, scale = scale), class = "scale_override")
}

CustomFacetWrap <- ggproto(
                           "CustomFacetWrap", FacetWrap,
                           init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
                               # make the initial x, y scales list
                               scales <- ggproto_parent(FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)

                               if(is.null(params$scale_overrides)) return(scales)

                               max_scale_x <- length(scales$x)
                               max_scale_y <- length(scales$y)

                               # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
                               for(scale_override in params$scale_overrides) {
                                   which <- scale_override$which
                                   scale <- scale_override$scale

                                   if("x" %in% scale$aesthetics) {
                                       if(!is.null(scales$x)) {
                                           if(which < 0 || which > max_scale_x) stop("Invalid index of x scale: ", which)
                                           scales$x[[which]] <- scale$clone()
                                       }
                                   } else if("y" %in% scale$aesthetics) {
                                       if(!is.null(scales$y)) {
                                           if(which < 0 || which > max_scale_y) stop("Invalid index of y scale: ", which)
                                           scales$y[[which]] <- scale$clone()
                                       }
                                   } else {
                                       stop("Invalid scale")
                                   }
                               }

                               # return scales
                               scales
                           }
                           )

facet_wrap_custom <- function(..., scale_overrides = NULL) {
      # take advantage of the sanitizing that happens in facet_wrap
      facet_super <- facet_wrap(...)
  
  # sanitize scale overrides
  if(inherits(scale_overrides, "scale_override")) {
          scale_overrides <- list(scale_overrides)
    } else if(!is.list(scale_overrides) || 
                          !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
            stop("scale_overrides must be a scale_override object or a list of scale_override objects")
      }
      
      facet_super$params$scale_overrides <- scale_overrides
      
      ggproto(NULL, CustomFacetWrap,
                  shrink = facet_super$shrink,
                      params = facet_super$params
                    )
}


#------------------------------------------------------------------------------
# Read in plaintiff's rebuttal report; clean up the numbers
#------------------------------------------------------------------------------
docurl <- "https://samv91khoyt2i553a2t1s05i-wpengine.netdna-ssl.com/wp-content/uploads/2018/06/Doc-415-2-Arcidiacono-Rebuttal-Report.pdf"

# import and clean up Table B.6.11R from rebuttal report
tb.6.11r <- extract_tables(docurl, pages=174, method = "stream") %>% pluck(1) %>% as_tibble %>%
            filter(V3!="") %>% select(-V5) %>%
            mutate(group=c("","African American","Hispanic","Asian American","African American","Hispanic","Asian American","","African American","Hispanic","Asian American","African American","Hispanic","Asian American")) %>%
            mutate(idx=rep(c("",rep("Observables Index",3),rep("Ordered Logit Coef.",3)),2)) %>%
            mutate(prat = c(rep("With",7),rep("Without",7))) %>%
            select(idx,group,prat,starts_with("V")) %>%
            rename(Overall = V1, Academic = V2, Extracurricular = V3, `Teacher 1` = V4, Counselor = V6, `Alumni Personal` = V7, `Alumni Overall` = V8, Personal = V9) %>%
            filter(group!="") %>%
            separate(`Teacher 1`, c("Teacher 1","Teacher 2"), sep=" ", extra = "merge") %>%
            mutate(Overall = str_replace_all(Overall,"[^0-9\\.-]", "")) %>%
            mutate_at(vars(Overall:Personal),~as.numeric(.)) %>% 
            mutate_if(is.character,~as.factor(.)) %>%
            mutate(group = fct_relevel(group, "African American","Hispanic")) %>%
            mutate(idx = fct_relevel(idx, "Observables Index","Ordered Logit Coef."))

# create two columns of numbers: ordered logit coefficients and basic z-scores
tb.6.11r %<>% pivot_longer(c("Overall","Academic","Extracurricular","Teacher 1","Teacher 2","Counselor","Alumni Personal","Alumni Overall","Personal"), names_to = "rating", values_to = "coef") %>%
              mutate(ratOrder = case_when(
                                          rating=="Overall" ~ 1,
                                          rating=="Alumni Overall" ~ 2,
                                          rating=="Academic" ~ 3,
                                          rating=="Extracurricular" ~ 4,
                                          rating=="Teacher 1" ~ 5,
                                          rating=="Teacher 2" ~ 6,
                                          rating=="Counselor" ~ 7,
                                          rating=="Alumni Personal" ~ 8,
                                          rating=="Personal" ~ 9,
                                          TRUE ~ 0)) #%>%
#              mutate(raceOrder = case_when(
#                                           group=="African American" ~ 1,
#                                           group=="Hispanic" ~ 2,
#                                           group=="Asian American" ~ 3,
#                                           TRUE ~ 0))
tb.6.11r.w <- tb.6.11r %>% pivot_wider(names_from = idx, values_from = coef)

# import and clean up Tables 5.1R, 5.4R, 5.5R, 5.6R, and 5.7R
t5.1r <- extract_tables(docurl, pages=108, method = "stream") %>% pluck(1) %>% as_tibble %>% slice(-seq(1,2)) %>% 
         rename(decile=V1, w.n=V2, b.n=V3, h.n=V4, a.n=V5, t.n=V6, w.shr=V7, b.shr=V8, h.shr=V9, a.shr=V10, t.shr=V11) %>%
         mutate_at(vars(w.n, b.n, h.n, a.n, t.n, w.shr, b.shr, h.shr, a.shr, t.shr),~as.numeric(gsub(",","",.)))

t5.4r <- extract_tables(docurl, pages=111, method = "stream") %>% pluck(1) %>% as_tibble %>% select(-V2) %>% slice(-1) %>% 
         rename(decile=V1, w.acad=V3, b.acad=V4, h.acad=V5, a.acad=V6, t.acad=V7, w.xtra=V8, b.xtra=V9, h.xtra=V10, a.xtra=V11, t.xtra=V12)

t5.5r <- extract_tables(docurl, pages=112, method = "stream") %>% pluck(1) %>% as_tibble %>% select(-V2) %>% slice(-1) %>% 
         rename(decile=V1, w.tch1=V3, b.tch1=V4, h.tch1=V5, a.tch1=V6, t.tch1=V7, w.tch2=V8, b.tch2=V9, h.tch2=V10, a.tch2=V11, t.tch2=V12, w.coun=V13, b.coun=V14, h.coun=V15, a.coun=V16, t.coun=V17)

t5.6r <- extract_tables(docurl, pages=113, method = "stream") %>% pluck(1) %>% as_tibble %>% select(-V2) %>% slice(-1) %>% 
         rename(decile=V1, w.pers=V3, b.pers=V4, h.pers=V5, a.pers=V6, t.pers=V7, w.alpr=V8, b.alpr=V9, h.alpr=V10, a.alpr=V11, t.alpr=V12)

t5.7r <- extract_tables(docurl, pages=114, method = "stream") %>% pluck(1) %>% as_tibble %>% select(-V2) %>% slice(-1) %>% 
         rename(decile=V1, w.over=V3, b.over=V4, h.over=V5, a.over=V6, t.over=V7, w.alov=V8, b.alov=V9, h.alov=V10, a.alov=V11, t.alov=V12)

rats  <- left_join(t5.4r,t5.5r,by="decile") %>% left_join(.,t5.6r,by="decile") %>% left_join(.,t5.7r,by="decile") %>% 
         mutate(decile = as.factor(decile)) %>% mutate_if(is.character,~as.numeric(str_remove_all(., '\\%'))) %>% 
         pivot_longer(
                      c("w.acad","b.acad","h.acad","a.acad","t.acad",
                        "w.xtra","b.xtra","h.xtra","a.xtra","t.xtra",
                        "w.tch1","b.tch1","h.tch1","a.tch1","t.tch1",
                        "w.tch2","b.tch2","h.tch2","a.tch2","t.tch2",
                        "w.coun","b.coun","h.coun","a.coun","t.coun",
                        "w.pers","b.pers","h.pers","a.pers","t.pers",
                        "w.alpr","b.alpr","h.alpr","a.alpr","t.alpr",
                        "w.over","b.over","h.over","a.over","t.over",
                        "w.alov","b.alov","h.alov","a.alov","t.alov"),
                      names_to = "r.rat", values_to = "pct2plus"
                     ) %>% 
         mutate(group = str_sub(r.rat, start=1, end=1),
                rating = str_sub(r.rat, start=3, end=6)) %>%
         select(decile,group,rating,pct2plus) %>% 
         mutate(group = replace(group,group=="w","White"),
                group = replace(group,group=="b","African American"),
                group = replace(group,group=="h","Hispanic"),
                group = replace(group,group=="a","Asian American"),
                group = replace(group,group=="t","Total"),
                rating = replace(rating,rating=="acad","Academic"),
                rating = replace(rating,rating=="xtra","Extracurricular"),
                rating = replace(rating,rating=="tch1","Teacher 1"),
                rating = replace(rating,rating=="tch2","Teacher 2"),
                rating = replace(rating,rating=="coun","Counselor"),
                rating = replace(rating,rating=="pers","Personal"),
                rating = replace(rating,rating=="alpr","Alumni Personal"),
                rating = replace(rating,rating=="over","Overall"),
                rating = replace(rating,rating=="alov","Alumni Overall"),
               ) %>% 
         mutate(ratOrder = case_when(
                                     rating=="Overall" ~ 1,
                                     rating=="Alumni Overall" ~ 2,
                                     rating=="Academic" ~ 3,
                                     rating=="Extracurricular" ~ 4,
                                     rating=="Teacher 1" ~ 5,
                                     rating=="Teacher 2" ~ 6,
                                     rating=="Counselor" ~ 7,
                                     rating=="Alumni Personal" ~ 8,
                                     rating=="Personal" ~ 9,
                                     TRUE ~ 0)) %>% 
         mutate(group = as.factor(group), rating = as.factor(rating), decile.num = as.numeric(as.character(decile)))

# import and clean up Tables B.5.1R, B.5.3R, B.5.4R, B.5.5R, and B.5.6R
tb.5.1r <- extract_tables(docurl, pages=147, method = "stream") %>% pluck(1) %>% as_tibble %>%
         rename(decile=V1, w.n=V2, b.n=V3, h.n=V4, a.n=V5, t.n=V6, w.shr=V7, b.shr=V8, h.shr=V9, a.shr=V10, t.shr=V11) %>%
         mutate_at(vars(w.n, b.n, h.n, a.n, t.n, w.shr, b.shr, h.shr, a.shr, t.shr),~as.numeric(gsub(",","",.)))

tb.5.3r <- extract_tables(docurl, pages=149, method = "stream") %>% pluck(1) %>% as_tibble %>% 
         rename(decile=V1, w.acad=V2, b.acad=V3, h.acad=V4, a.acad=V5, t.acad=V6, w.xtra=V7, b.xtra=V8, h.xtra=V9, a.xtra=V10, t.xtra=V11) %>%
         mutate_all(~as.numeric(gsub("%","",.)))

tb.5.4r <- extract_tables(docurl, pages=150, method = "stream") %>% pluck(1) %>% as_tibble %>% 
         rename(decile=V1, w.tch1=V2, b.tch1=V3, h.tch1=V4, a.tch1=V5, t.tch1=V6, w.tch2=V7, b.tch2=V8, h.tch2=V9, a.tch2=V10, t.tch2=V11, w.coun=V12, b.coun=V13, h.coun=V14, a.coun=V15, t.coun=V16) %>%
         mutate_all(~as.numeric(gsub("%","",.)))

tb.5.5r <- extract_tables(docurl, pages=151, method = "stream") %>% pluck(1) %>% as_tibble %>% 
         rename(decile=V1, w.pers=V2, b.pers=V3, h.pers=V4, a.pers=V5, t.pers=V6, w.alpr=V7, b.alpr=V8, h.alpr=V9, a.alpr=V10, t.alpr=V11) %>%
         mutate_all(~as.numeric(gsub("%","",.)))

tb.5.6r <- extract_tables(docurl, pages=152, method = "stream") %>% pluck(1) %>% as_tibble %>%
         rename(decile=V1, w.over=V2, b.over=V3, h.over=V4, a.over=V5, t.over=V6, w.alov=V7, b.alov=V8, h.alov=V9, a.alov=V10, t.alov=V11) %>%
         mutate_all(~as.numeric(gsub("%","",.)))

#------------------------------------------------------------------------------
# Compute number of LDC by AI decile and then the same for each rating
#------------------------------------------------------------------------------
N.LDC.AI <- (tb.5.1r %>% mutate_all(~as.numeric(.))) - (t5.1r %>% mutate_all(~as.numeric(.)))
N.LDC.AI %<>% as_tibble %>% select(-w.shr,-b.shr,-h.shr,-a.shr,-t.shr)

# Academic Rating
temp1 <- tb.5.1r %>% select(-contains("shr")) %>% mutate_all(~as.numeric(.))
temp2 <- tb.5.3r %>% select(-contains("xtra"))
temp3 <- t5.1r %>% select(-contains("shr")) %>% mutate_all(~as.numeric(.))
temp4 <- t5.4r %>% select(-contains("xtra")) %>% mutate_all(~as.numeric(gsub("%","",.)))
N.acad.2p.AI <- round(temp2/100 * temp1) - round(temp4/100 * temp3)
N.acad.2p.AI %<>% as_tibble
LDC.acad.2p.AI <- N.acad.2p.AI/N.LDC.AI*100
LDC.acad.2p.AI %<>% as_tibble %>% mutate(decile = c(as.character(seq(1,10)),"Average"))
LDC.acad.2p.AI[2,"t.acad"] <- 0

# Extracurricular Rating
temp2 <- tb.5.3r %>% select(-contains("acad"))
temp4 <- t5.4r %>% select(-contains("acad")) %>% mutate_all(~as.numeric(gsub("%","",.)))
N.xtra.2p.AI <- round(temp2/100 * temp1) - round(temp4/100 * temp3)
N.xtra.2p.AI %<>% as_tibble
LDC.xtra.2p.AI <- N.xtra.2p.AI/N.LDC.AI*100
LDC.xtra.2p.AI %<>% as_tibble %>% mutate(decile = c(as.character(seq(1,10)),"Average"))

# Personal Rating
temp2 <- tb.5.5r %>% select(-contains("alpr"))
temp4 <- t5.6r %>% select(-contains("alpr")) %>% mutate_all(~as.numeric(gsub("%","",.)))
N.pers.2p.AI <- round(temp2/100 * temp1) - round(temp4/100 * temp3)
N.pers.2p.AI %<>% as_tibble
LDC.pers.2p.AI <- N.pers.2p.AI/N.LDC.AI*100
LDC.pers.2p.AI %<>% as_tibble %>% mutate(decile = c(as.character(seq(1,10)),"Average"))

# Alumni Personal Rating
temp2 <- tb.5.5r %>% select(-contains("pers"))
temp4 <- t5.6r %>% select(-contains("pers")) %>% mutate_all(~as.numeric(gsub("%","",.)))
N.alpr.2p.AI <- round(temp2/100 * temp1) - round(temp4/100 * temp3)
N.alpr.2p.AI %<>% as_tibble
LDC.alpr.2p.AI <- N.alpr.2p.AI/N.LDC.AI*100
LDC.alpr.2p.AI %<>% as_tibble %>% mutate(decile = c(as.character(seq(1,10)),"Average"))

# Overall Rating
temp2 <- tb.5.6r %>% select(-contains("alov"))
temp4 <- t5.7r %>% select(-contains("alov")) %>% mutate_all(~as.numeric(gsub("%","",.)))
N.over.2p.AI <- round(temp2/100 * temp1) - round(temp4/100 * temp3)
N.over.2p.AI %<>% as_tibble
LDC.over.2p.AI <- N.over.2p.AI/N.LDC.AI*100
LDC.over.2p.AI %<>% as_tibble %>% mutate(decile = c(as.character(seq(1,10)),"Average"))

# Alumni Overall Rating
temp2 <- tb.5.6r %>% select(-contains("over"))
temp4 <- t5.7r %>% select(-contains("over")) %>% mutate_all(~as.numeric(gsub("%","",.)))
N.alov.2p.AI <- round(temp2/100 * temp1) - round(temp4/100 * temp3)
N.alov.2p.AI %<>% as_tibble
LDC.alov.2p.AI <- N.alov.2p.AI/N.LDC.AI*100
LDC.alov.2p.AI %<>% as_tibble %>% mutate(decile = c(as.character(seq(1,10)),"Average"))

# Teacher 1 Rating
temp2 <- tb.5.4r %>% select(-contains("tch2"),-contains("coun"))
temp4 <- t5.5r %>% select(-contains("tch2"),-contains("coun")) %>% mutate_all(~as.numeric(gsub("%","",.)))
N.tch1.2p.AI <- round(temp2/100 * temp1) - round(temp4/100 * temp3)
N.tch1.2p.AI %<>% as_tibble
LDC.tch1.2p.AI <- N.tch1.2p.AI/N.LDC.AI*100
LDC.tch1.2p.AI %<>% as_tibble %>% mutate(decile = c(as.character(seq(1,10)),"Average"))

# Teacher 2 Rating
temp2 <- tb.5.4r %>% select(-contains("tch1"),-contains("coun"))
temp4 <- t5.5r %>% select(-contains("tch1"),-contains("coun")) %>% mutate_all(~as.numeric(gsub("%","",.)))
N.tch2.2p.AI <- round(temp2/100 * temp1) - round(temp4/100 * temp3)
N.tch2.2p.AI %<>% as_tibble
LDC.tch2.2p.AI <- N.tch2.2p.AI/N.LDC.AI*100
LDC.tch2.2p.AI %<>% as_tibble %>% mutate(decile = c(as.character(seq(1,10)),"Average"))

# Teacher 1 Rating
temp2 <- tb.5.4r %>% select(-contains("tch2"),-contains("tch1"))
temp4 <- t5.5r %>% select(-contains("tch2"),-contains("tch1")) %>% mutate_all(~as.numeric(gsub("%","",.)))
N.coun.2p.AI <- round(temp2/100 * temp1) - round(temp4/100 * temp3)
N.coun.2p.AI %<>% as_tibble
LDC.coun.2p.AI <- N.coun.2p.AI/N.LDC.AI*100
LDC.coun.2p.AI %<>% as_tibble %>% mutate(decile = c(as.character(seq(1,10)),"Average"))

ratsLDC <- left_join(LDC.acad.2p.AI,LDC.xtra.2p.AI,by="decile") %>% left_join(.,LDC.pers.2p.AI,by="decile") %>% left_join(.,LDC.over.2p.AI,by="decile") %>%
           left_join(.,LDC.alpr.2p.AI,by="decile") %>% left_join(.,LDC.alov.2p.AI,by="decile") %>% left_join(.,LDC.tch1.2p.AI,by="decile") %>% 
           left_join(.,LDC.tch2.2p.AI,by="decile") %>% left_join(.,LDC.coun.2p.AI,by="decile") %>% 
         mutate(decile = as.factor(decile)) %>%
         pivot_longer(
                      c("w.acad","b.acad","h.acad","a.acad","t.acad",
                        "w.xtra","b.xtra","h.xtra","a.xtra","t.xtra",
                        "w.tch1","b.tch1","h.tch1","a.tch1","t.tch1",
                        "w.tch2","b.tch2","h.tch2","a.tch2","t.tch2",
                        "w.coun","b.coun","h.coun","a.coun","t.coun",
                        "w.pers","b.pers","h.pers","a.pers","t.pers",
                        "w.alpr","b.alpr","h.alpr","a.alpr","t.alpr",
                        "w.over","b.over","h.over","a.over","t.over",
                        "w.alov","b.alov","h.alov","a.alov","t.alov"),
                      names_to = "r.rat", values_to = "pct2plus"
                     ) %>% 
         mutate(group = str_sub(r.rat, start=1, end=1),
                rating = str_sub(r.rat, start=3, end=6)) %>%
         select(decile,group,rating,pct2plus) %>% 
         mutate(group = replace(group,group=="w","White"),
                group = replace(group,group=="b","African American"),
                group = replace(group,group=="h","Hispanic"),
                group = replace(group,group=="a","Asian American"),
                group = replace(group,group=="t","Total"),
                rating = replace(rating,rating=="acad","Academic"),
                rating = replace(rating,rating=="xtra","Extracurricular"),
                rating = replace(rating,rating=="tch1","Teacher 1"),
                rating = replace(rating,rating=="tch2","Teacher 2"),
                rating = replace(rating,rating=="coun","Counselor"),
                rating = replace(rating,rating=="pers","Personal"),
                rating = replace(rating,rating=="alpr","Alumni Personal"),
                rating = replace(rating,rating=="over","Overall"),
                rating = replace(rating,rating=="alov","Alumni Overall"),
               ) %>% 
         mutate(ratOrder = case_when(
                                     rating=="Overall" ~ 1,
                                     rating=="Alumni Overall" ~ 2,
                                     rating=="Academic" ~ 3,
                                     rating=="Extracurricular" ~ 4,
                                     rating=="Teacher 1" ~ 5,
                                     rating=="Teacher 2" ~ 6,
                                     rating=="Counselor" ~ 7,
                                     rating=="Alumni Personal" ~ 8,
                                     rating=="Personal" ~ 9,
                                     TRUE ~ 0)) %>% 
         mutate(group = as.factor(group), rating = as.factor(rating), decile.num = as.numeric(as.character(decile)))

bigrats <- bind_rows(rats %>% mutate(special = "Non-ALDC"),ratsLDC %>% mutate(special = "LDC")) %>% mutate(special = as.factor(special))

#------------------------------------------------------------------------------
# FIGURE 1: COLOR line plots of likelihood of receiving a 2 or better on various ratings, all races (facet wrap)
#------------------------------------------------------------------------------
# Reorder some of the factors for convenience
rats %<>% mutate(Group = fct_relevel(group, "Asian American","White","Hispanic","African American"))
rats %<>% mutate(dec2   = fct_relevel(decile, "1","2","3","4","5","6","7","8","9","10"))
rats %<>% mutate(rat2   = fct_relevel(rating, "Academic","Personal","Extracurricular","Overall","Teacher 1","Teacher 2","Counselor","Alumni Overall","Alumni Personal"))
levels(rats$dec2) <- c("Decile 1","Decile 2","Decile 3","Decile 4","Decile 5","Decile 6","Decile 7","Decile 8","Decile 9","Decile 10","Average")

# Try with custom function overriding facet_wrap
g <- ggplot(data=rats %>% filter((group %in% c("White","African American","Hispanic","Asian American")) & (decile!="Average") & rating %in% c("Academic","Personal","Overall","Extracurricular")), aes(x=as.numeric(dec2), y=pct2plus))
g + geom_line(aes(color=Group)) +
    labs(x = "Academic Index Decile", y = "Percent Receiving 2 or Better", linetype = NULL) + theme_minimal() +  
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) + 
    facet_wrap_custom(~rat2, scales="free_y", nrow=2, scale_overrides=scale_override(1, scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by=20)))) + scale_x_continuous(breaks=seq(1,10), labels=c("1","2","3","4","5","6","7","8","9","10")) + scale_color_colorblind() + scale_y_continuous(limits = c(0,50), breaks = seq(0,50,by=10))
ggsave("../../Figures/Figure1.pdf", device = "pdf", width=7.75, height = 6, units = "in")


# #------------------------------------------------------------------------------
# # COLOR line plots of likelihood of receiving a 2 or better on various ratings, all races (separately for each rating)
# #------------------------------------------------------------------------------
# # Academic rating
# g <- ggplot(data=rats %>% filter((group %in% c("White","African American","Hispanic","Asian American")) & (decile!="Average") & rating %in% c("Academic")), aes(x=as.numeric(dec2), y=pct2plus))
# g + geom_line(aes(color=Group)) +
#     labs(x = "Academic Index Decile", y = "Percent Receiving 2 or Better", linetype = NULL) + theme_minimal() +  
#     theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position = "bottom", panel.border = element_rect(color="black", fill=NA)) + 
#     scale_x_continuous(breaks=seq(1,10), labels=c("10","9","8","7","6","5","4","3","2","1")) + scale_color_colorblind() + scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by=20))
# ggsave("../../Figures/Figure1acad.pdf", device = "pdf", width=7.75, height = 6, units = "in")
# 
# # Extracurricular rating
# g <- ggplot(data=rats %>% filter((group %in% c("White","African American","Hispanic","Asian American")) & (decile!="Average") & rating %in% c("Extracurricular")), aes(x=as.numeric(dec2), y=pct2plus))
# g + geom_line(aes(color=Group)) +
#     labs(x = "Academic Index Decile", y = "Percent Receiving 2 or Better", linetype = NULL) + theme_minimal() +  
#     theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position = "bottom", panel.border = element_rect(color="black", fill=NA)) + 
#     scale_x_continuous(breaks=seq(1,10), labels=c("10","9","8","7","6","5","4","3","2","1")) + scale_color_colorblind() + scale_y_continuous(limits = c(0,50), breaks = seq(0,50,by=10))
# ggsave("../../Figures/Figure1xtra.pdf", device = "pdf", width=7.75, height = 6, units = "in")
# 
# # Personal rating
# g <- ggplot(data=rats %>% filter((group %in% c("White","African American","Hispanic","Asian American")) & (decile!="Average") & rating %in% c("Personal")), aes(x=as.numeric(dec2), y=pct2plus))
# g + geom_line(aes(color=Group)) +
#     labs(x = "Academic Index Decile", y = "Percent Receiving 2 or Better", linetype = NULL) + theme_minimal() +  
#     theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position = "bottom", panel.border = element_rect(color="black", fill=NA)) + 
#     scale_x_continuous(breaks=seq(1,10), labels=c("10","9","8","7","6","5","4","3","2","1")) + scale_color_colorblind() + scale_y_continuous(limits = c(0,50), breaks = seq(0,50,by=10))
# ggsave("../../Figures/Figure1pers.pdf", device = "pdf", width=7.75, height = 6, units = "in")
# 
# # Overall rating
# g <- ggplot(data=rats %>% filter((group %in% c("White","African American","Hispanic","Asian American")) & (decile!="Average") & rating %in% c("Overall")), aes(x=as.numeric(dec2), y=pct2plus))
# g + geom_line(aes(color=Group)) +
#     labs(x = "Academic Index Decile", y = "Percent Receiving 2 or Better", linetype = NULL) + theme_minimal() +  
#     theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position = "bottom", panel.border = element_rect(color="black", fill=NA)) + 
#     scale_x_continuous(breaks=seq(1,10), labels=c("10","9","8","7","6","5","4","3","2","1")) + scale_color_colorblind() + scale_y_continuous(limits = c(0,50), breaks = seq(0,50,by=10))
# ggsave("../../Figures/Figure1ovrl.pdf", device = "pdf", width=7.75, height = 6, units = "in")
# 


#------------------------------------------------------------------------------
# FIGURE 3: bar plots of ordered logit results, all races (facet wrap)
#------------------------------------------------------------------------------
# bar plots of Z-scores and ordered logit coefficients by race
tb.6.11r %<>% mutate(Group = fct_relevel(group, "Asian American","White","Hispanic","African American"))
tb.6.11r %<>% mutate(rat2   = fct_relevel(rating, "Academic","Personal","Extracurricular","Overall","Teacher 1","Teacher 2","Counselor","Alumni Overall","Alumni Personal"))

# bar plots of Z-scores and ordered logit coefficients by race (legend on bottom)
g <- ggplot(data=tb.6.11r %>% filter((group %in% c("White","African American","Hispanic","Asian American")) & (prat=="With") & (rating %in% c("Academic","Personal","Overall","Extracurricular"))), aes(x=Group, y=coef, fill=idx))
g + geom_col(position = "dodge", color = "black", size=0.05) +
    labs(x = NULL, y = "Index or Coefficient (Relative to Whites)", fill = NULL) + theme_bw() +
        theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(color = "black"), axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position = "bottom") +
            scale_fill_grey(start=1,end=0.75) +
                facet_wrap(~rat2, scales="fixed", nrow=2) + theme(strip.background = element_blank(), strip.placement = "outside", strip.text.y = element_text(angle = 0, color = "black")) + scale_y_continuous(limits = c(-1.55, 1.55), breaks = seq(-1.5,1.5,by=0.5))

ggsave("../../Figures/OAFigure3.pdf", device = "pdf", width=7.75, height = 6, units = "in")
