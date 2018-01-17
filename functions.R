



load_log <- function(user = NA) {
  log <- read_csv("log.csv")
  if (is.na(user)) return(log) else return(log %>% filter(person == user))
}




add_to_log <- function(person, date, weight) {
  input <- data.frame(person = person, date = as.Date(date), weight = weight)
  log <- load_log()
  log2 <- rbind(log, input)
  dups <- duplicated(log2)
  if (tail(dups, 1)) write_csv(log, "log.csv") else write_csv(log2, "log.csv")
  
}



process_log <- function(log, who, days_to_average = 7) {
  ## add to front of log so that rollmean and roll sum will always work
  
  # if there are multiple entries for a given date, we ignore the more recent one (since entries appear in the order they were entered)
  log <- log %>% filter(person == who) %>% filter(!duplicated(date))
  
  
  append <- data.frame(person = who, date = seq(min(log$date) - days(8), min(log$date) - days(1),
                                                by = "days"),
                       weight = log$weight[log$date == min(log$date)])
  log <- rbind(append, log)
  
  log %>% 
    mutate(source = "scale") %>% 
    right_join(data.frame(date = seq(from = min(log$date), to = max(log$date), "days"))) %>% 
    arrange(desc(date)) %>% 
    mutate(source = ifelse(is.na(source), "fill", "scale"),
           person = who,
           weight = na.approx(weight),
           moving_average = rollmean(weight, days_to_average, fill = "extend", align = "left") %>% round(2),
           daily_weight_loss = (moving_average - lead(moving_average)) * -1,
           weight_loss_past_7_days = rollsum(daily_weight_loss, 7, fill = "extend", align = "left") %>% round(2)
    )
  
  
}


summarize_all <- function(log, users) {
  dat <- map_df(names(users), function(x) {
    process_log(log = log, who = x)
  })
  return(dat)
}

get_current_weight <- function(log, users) {
  all <- summarize_all(log, users)
  current_weights <-  map_dbl(names(users), function(x) {
    all %>% filter(person == x) %>% filter(date == max(date)) %>% 
      pull(moving_average)
  })
  names(current_weights) <- names(users)
  return(current_weights)
}


compute_progress <- function(initial, target, current) {
  weight_loss <- initial - current
  goal <- initial - target
  return((weight_loss / goal) * 100)
  
}



plot_progress <- function(log, users) {
  all <- summarize_all(log, users)
  initials <- map_dbl(users, "initial")
  targets <- map_dbl(users, "target")
  currents <- get_current_weight(log, users)
  pounds_lost <- (initials - currents) %>% round(2)
  
  progress <- pmap_dbl(.l = list(initials, targets, currents), function(a,b,c) {
    return(compute_progress(a, b, c))
  })
  
  progress <- progress %>% enframe() %>% mutate(pounds_lost = pounds_lost)
  
  breaks = seq(20, 100, 20)
  labels = paste0(breaks, "%")
  p <- progress %>% select(name, progress = value, pounds_lost) %>% 
    ggplot(aes(x = name, y = progress, fill = name)) + geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(breaks = breaks, labels = labels, limits = c(0,100)) +
    geom_text(aes(x = name, y = progress, label = pounds_lost), size = 3, vjust = 0) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(p)
  
}



plot_weights <- function(log, users) {
  
  all <- summarize_all(log, users)
  all %>% ggplot(aes(x = date, y = moving_average, color = person)) + 
    geom_line() +
    coord_cartesian(xlim = c(as.Date("2018-01-09"), as.Date("2018-05-01")),
                    ylim = c(100, max(all$weight))) +
    theme_bw()
  
  
}


