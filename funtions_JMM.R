halfDL<-function(x){
  if_else(str_detect(as.character(x),"<") == "TRUE",
          map_chr(x,function(x){
            x<-str_replace_all(as.character(x),"<","") 
            as.numeric(x) * 0.5}),
          as.character(x))
} 
# 
# DL<-function(x){
#   if_else(str_detect(as.character({{x}}),"<") == "TRUE",
#           map_chr({{x}},function(y){
#             y<-str_replace_all(as.character(y),"<","") 
#             as.numeric(y) }),
#           as.character({{x}}))
# } 

DL<-function(x){
  if_else(str_detect(as.character(x),"<") == "TRUE",
          map_chr(x,function(x){
            x<-str_replace_all(as.character(x),"<","") 
            as.numeric(x) }),
          as.character(x))
} 

DL.5<-function(x){
  if_else(str_detect(as.character(x),"<") == "TRUE",
          map_chr(x,function(x){
            x<-str_replace_all(as.character(x),"<","") 
            as.numeric(x) * 5}),
          as.character(x))
} 


compute_DL <- function(df,x){
  
  DL_col <- df %>% 
    mutate(Raw.DLAdjusted=ifelse(str_detect(as.character(df[[x]]),"<"),
                                 as.numeric(str_replace_all(as.character(df[[x]]),"<","")),
                                 as.numeric(as.character(df[[x]])))
    )
    
  
  return(DL_col)
}

compute_half_DL <- function(df,x){
  
  DL_col <- df %>% 
    mutate(Raw.DL5Adjusted=ifelse(str_detect(as.character(df[[x]]),"<"),
                                 as.numeric(str_replace_all(as.character(df[[x]]),"<",""))*0.5,
                                 as.numeric(as.character(df[[x]]))),
           DL.Flag = ifelse(str_detect(as.character(df[[x]]),"<"),1,0)
           
    )
  
  
  return(DL_col)
}



get_historical_data <- function(df,param_col,hist_sites){
  
  df2 <- df %>%
    mutate(Date = as.Date(Date))%>%
    select (Site,Date,Year,Month,{{param_col}})%>%
    filter(Year >= 2009, Year <= 2018)%>%
    filter(Site %in% hist_sites)
  
  hist.data <- df2 %>%
    mutate(Raw.DLAdjusted = DL(df2[[param_col]]), ## using the DL for the percentile calculation
           Raw.DL5Adjusted = DL.5(df2[[param_col]]), ## using 5x DL for percentile calculation 
           DL.Flag= ifelse(grepl("<",df2[[param_col]])==TRUE,1,0))
  
  
  return(hist.data)
  
}

compute_trigger1 <- function(df,prob){
  
  
  df %>% 
    group_by(Site) %>%
    summarise(x=list(enframe(quantile(as.numeric(Raw.DLAdjusted), probs=prob), "quantiles", "Raw.DLAdjusted"))) %>% 
    unnest(x)
  
  
}

compute_trigger2 <- function(df,prob){
  
  
  df %>% 
    group_by(Site) %>%
    summarise(x=list(enframe(quantile(as.numeric(Raw.DL5Adjusted), probs=prob), "quantiles", "Raw.DL5Adjusted"))) %>% 
    unnest(x)
  
  
  
  
}

compute_trigger_exceedance <- function(level_fac){
  
  
  trigger.exceedance %>% 
    #filter(Month ==	s.Month) %>% 
    select(Month,Site, variable, {{level_fac}})  %>%
    filter(!is.na({{level_fac}})) %>% 
    mutate(variable = factor(variable, levels = c("pH.lower", "pH.upper","Sulphate", "Cu.Dissolved", "Fe.Dissolved", "Zn.Dissolved")))  %>%
    arrange(Month,Site, variable) %>%
    spread(variable, {{level_fac}})
}

plot_graphs <- function(hist_df){

  p1 <- ggplot(hist_df, aes(x=Date, y=Raw.DLAdjust))+
    geom_point(aes(shape=Month,color=Month)) +
    
    scale_x_date(limits=c(as.Date("2009-01-01"),to_date),date_breaks = "years", date_labels = "%Y",expand = c(0,0))+
    
    coord_cartesian(clip="off")+
    scale_y_continuous(limits= c(ymin,ymax))+
    
    scale_shape_manual(values=pts)+
    scale_color_manual(values=col)+
    xlab("Date")+                                                                                           #####x-axis label.
    ylab(ylabel)+
    ggtitle(site) 
  
    
  
}


WQ.Plot.refline <- function (plotdat,grps,trigger1, trigger2,Up.ref, ylabel, ymin,ymax,
                             ref.label,to_date, is.ref, is.trigger, 
                             text1.line=NA,ref.line=NA){
  
  
  for.legend <- legend.info %>%
    filter(Month %in% unique(plotdat$Month))
  
  pts <- for.legend$month.pts
  col <- for.legend$month.colour
  
  
  
  p1 <- ggplot(plotdat, aes(x=Date, y=Raw.DLAdjust))+
    geom_point(aes(shape=Month,color=Month)) +
    
    scale_x_date(limits=c(as.Date("2009-01-01"),to_date),date_breaks = "years", date_labels = "%Y",expand = c(0,0))+
    
    coord_cartesian(clip="off")+
    scale_y_continuous(limits= c(ymin,ymax))+
    
    scale_shape_manual(values=pts)+
    scale_color_manual(values=col)+
    xlab("Date")+                                                                                           #####x-axis label.
    ylab(ylabel)
    #ggtitle(unique(plotdat$Site))
  
  
  
  
  if(is.ref){
    
    if(is.na(ref.line)){
      ref.text_table <- tibble(Month=s.Month,
                               Site= unique(plotdat$Site),
                               Variable = unique(plotdat$Parameter),
                               group.num = grps,
                               x.text = to_date,
                               ref.l = Up.ref$Raw.DLAdjust[nrow(Up.ref)],
                               label = ref.label)
      
      #ref.l <- as.numeric(Up.ref$Raw.DLAdjust[nrow(Up.ref)])
    }else{
      ref.l <- as.numeric(ref.line)
    }
    
    up.ref.mod_rbind=tibble()
  
    for(i in 1:length(unique(plotdat$Site))){
      up.ref.mod <- Up.ref %>% mutate(Site = unique(plotdat$Site)[i])
      up.ref.mod_rbind <- bind_rows(up.ref.mod_rbind,up.ref.mod)
    }
    
    
    p1 <- p1+  
      geom_line(data=up.ref.mod_rbind,aes(x=Date,y=Raw.DLAdjust),lty="dotted",colour="purple")+
      geom_text(data=ref.text_table, aes(x.text, ref.l, label = label), hjust=-0.1,size=2.5,colour="purple")
      
  }
  
  
  
  if(is.trigger){
    
    ph_label <- case_when(unique(plotdat$Parameter) == 'pH' ~ c("(U)","(L)"),
                          TRUE ~ '')
    
    if(is.na(text1.line)){
      trigger.text_table<- trigger1 %>% 
        mutate(x.text = to_date,
          trigger.t = `Trigger Level`,
               label= ifelse(unique(plotdat$Parameter) == 'pH', 
                             paste0("Level 1 Trigger ",  c("(U)","(L)")," = ",as.character(round(trigger.t,3))),
                             paste0("Level 1 Trigger ", " = ",as.character(round(trigger.t,3))))
              )
            
    }else{
      trigger.text_table <- as.numeric(text1.line)
    }
    
    trigger2.text_table <- trigger2 %>% 
      mutate(x.text = to_date,
             trigger2.t = ifelse(`Trigger Level`> ymax, as.numeric(ymax),`Trigger Level`),
             label= ifelse(unique(plotdat$Parameter) == 'pH', 
                           paste0("Level 2 Trigger ",  c("(U)","(L)")," = ",as.character(round(trigger2.t,3))),
                           paste0("Level 2 Trigger ", " = ",as.character(round(trigger2.t,3))))
      )        
      
                                                                    
                                                                   
    
    
    # if(any(trigger2 > as.numeric(ymax))){
    #   trigger2.t <- as.numeric(ymax)
    # }else{
    #   trigger2.t <- trigger2
    # }
    # 
    
    
  p1 <- p1+
    geom_hline(data=trigger.text_table, aes(yintercept= trigger.t),lty="dotted",col="gray13")+
    geom_text(data= trigger.text_table, aes(x.text, trigger.t,label=label), 
              hjust=-0.1,size=2.5,colour="gray13", check_overlap = TRUE)+
  
    geom_hline(data=trigger2.text_table, aes(yintercept= trigger2.t),lty="dotted",col="gray13")+
    geom_text(data= trigger2.text_table, aes(x.text, trigger2.t,label=label), 
              hjust=-0.1,size=2.5,colour="gray13",check_overlap = TRUE)
    
  
  }

    p1 <- p1+
      facet_wrap(~Site)+
      graph.theme
  
    
  
  return(p1)
} 



WQ.Plot.refline.log <- function (plotdat,site,trigger, trigger.2,Up.ref, ylabel, xmin,xmax,text1.line=NA,ref.label= "JM11 (reference)",ref.line=NA){
  
  if(is.na(text1.line)){
    trigger.t <- as.numeric(trigger)
  }else{
    trigger.t <- as.numeric(text1.line)
  }
  
  if(is.na(ref.line)){
    ref.l <- as.numeric(Up.ref$Raw.DLAdjust[nrow(Up.ref)])
  }else{
    ref.l <- as.numeric(ref.line)
  }
  
  if(as.numeric(trigger.2) > as.numeric(xmax)){
    trigger2.t <- as.numeric(xmax)
  }else{
    trigger2.t <- as.numeric(trigger.2)
  }
  
  for.legend <- legend.info %>%
    filter(Month %in% unique(plotdat$Month))
  
  pts <- for.legend$month.pts
  col <- for.legend$month.colour
  
  ggplot(plotdat, aes(x=Date, y=Raw.DLAdjust))+
    geom_point(aes(shape=Month,color=Month)) +
    geom_line(data=Up.ref,aes(x=Date,y=Raw.DLAdjust),lty="dotted",colour="purple")+
    scale_x_date(limits=c(as.Date("2009-01-01"),as.Date("2020-12-31")),date_breaks = "years", date_labels = "%Y",expand = c(0,0))+
    
    coord_cartesian(clip="off")+
    scale_y_log10() +
    annotation_logticks()+ ##for log scale
    
    scale_shape_manual(values=pts)+
    scale_color_manual(values=col)+
    
    
    xlab("Date")+                                                                                           #####x-axis label.
    ylab(ylabel)+
    ggtitle(site) +
    
    geom_hline(aes(yintercept=as.numeric(trigger)),lty="dotted",col="gray13")+ ###trigger 1
    annotate("text",x=as.Date("2020-12-31"),y=trigger.t,label=paste("Level 1 Trigger = ",as.character(trigger)),hjust=-0.1,size=2.5,colour="gray13")+
    
    geom_hline(aes(yintercept=as.numeric(trigger.2)),lty="dotted",col="gray13")+ ##trigger 2
    annotate("text",x=as.Date("2020-12-31"),y=trigger2.t,label=paste("Level 2 Trigger = ",as.character(trigger.2)),hjust=-0.1,size=2.5,colour="gray13")+
    
    annotate("text",x=as.Date("2020-12-31"),y=ref.l,label=ref.label,hjust=-0.1,size=2.5,colour="purple")+
    
    
    graph.theme
} 



create_plot_data <- function(df,site, para){
  
  df %>%
    filter(Year >= 2009, Site == {{site}}, Parameter== {{para}})%>%
    ungroup(Month)%>%
    mutate(Month = recode(Month,'April' = "April",
                          'Jun' = "June",
                          'Jul' = "July",
                          'Aug' = "August",
                          'Sep' = "September",
                          'Oct' = "October")) %>%
    mutate (Month= factor(Month, levels = c("June", "July", "August","September","October")))
  
  
  
  
}


create_plot_data_table <- function(df,grps, para){
  
  df %>%
    filter(Year >= 2009, group.num == grps, Parameter== para)%>%
    ungroup(Month)%>%
    mutate(Month = recode(Month,'April' = "April",
                          'Jun' = "June",
                          'Jul' = "July",
                          'Aug' = "August",
                          'Sep' = "September",
                          'Oct' = "October")) %>%
    mutate (Month= factor(Month, levels = c("June", "July", "August","September","October")))
  
  
  
  
}



compute_triggers_reflines <- function(T.1,T.2,site,para,ref_site){
                                      
  
  
  trigger.1 <- T.1 %>%
    filter(Site == {{site}}, Variable == {{para}}) %>%
    select(`Trigger Level`)
  
  trigger.2 <- T.2 %>%
    filter(Site== {{site}} ,Variable ==  {{para}}) %>%
    select(`Trigger Level`)
  
  #Upstream Reference (JM11)
  Up.ref <- WQ.all.plots %>%
    filter (Year >= 2009,Site== {{ref_site}}, Parameter== {{para}})
  
  
  return(list(trigger.1,trigger.2, Up.ref))
  
}



compute_triggers <- function(T.1,T.2,site,para){
  
  
  
  trigger.1 <- T.1 %>%
    filter(Site == {{site}}, Variable == {{para}}) %>%
    select(`Trigger Level`)
  
  trigger.2 <- T.2 %>%
    filter(Site== {{site}} ,Variable ==  {{para}}) %>%
    select(`Trigger Level`)
  
  
  return(list(trigger.1,trigger.2))
  
}

compute_triggers_table <- function(T.1,T.2,sites,para){
  
  
  
  trigger.1 <- T.1 %>%
    filter(Site %in% sites, Variable == para) 
  
  trigger.2 <- T.2 %>%
    filter(Site %in% sites ,Variable ==  para)
  
  return(list(trigger.1,trigger.2))
  
}

compute_reference_site <- function(ref_site, para){
  
   WQ.all.plots %>%
    filter (Year >= 2009,Site== {{ref_site}}, Parameter== {{para}})
  
  
}

compute_reference_sites <- function(ref_site, para){
  
  WQ.all.plots %>%
    filter (Year >= 2009,Site== ref_site, Parameter== para)
  
  
}

compute_triggers_pH <- function(df){
  
  return(df)
}

Assign_group.num <- function(df){
  
  df %>% mutate(group.num = case_when(Site %in% c("JM5", "JM6") ~ "Group1",
                                      Site == "JM7" ~ "Group2",
                                      Site %in% c("JM1", "JM2", "JM3", "JM4") ~ "Group3",
                                      TRUE ~ "NA"))
  
  
}
