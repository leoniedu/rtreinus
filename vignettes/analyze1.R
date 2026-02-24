devtools::load_all()

session <- treinus_auth()
## current (2026-01-29) max(id_athlete)=56
exercises_new <- treinus_get_exercises(session, athlete_id = 1:70)

exercises <- treinus_get_exercises_db()
exercises_today <- exercises%>%
  filter(as.Date(start)==Sys.Date(), start_time_as_string>="05:00", start_time_as_string<="07:00", !is.na(speed))

exercise_analysis <- purrr::pmap(exercises_today, function(id_exercise, id_athlete, ...)  treinus_get_exercise_analysis(session, exercise_id=id_exercise, athlete_id=id_athlete))

exercise_records <- purrr::map(exercise_analysis, ~tibble(id_athlete=.x$data$Analysis$IdAthlete,
  id_exercise=.x$data$Analysis$IdExercise,
  fullname_athlete=.x$data$Analysis$User$FullName,treinus_extract_records(.x)))%>%purrr::list_rbind()

library(sf)

exercise_records_sf <- exercise_records%>%
  filter(!is.na(position_long))%>%
  mutate(
timestamp=as.POSIXct(timestamp),
    lat=position_lat* 180 / 2^31,
lon=position_long * 180 / 2^31
  )%>%
  st_as_sf(coords=c("lon", "lat"), remove = FALSE, crs=4326)%>%
  st_transform(crs=31984)

fast500 <- fastest_straight_distance(sf_points = exercise_records_sf, athlete_col = "id_athlete", time_col = "timestamp", distance_m = 100)%>%
  left_join(exercise_records_sf%>%st_drop_geometry()%>%distinct(id_athlete,fullname_athlete))

View(fast500%>%

fast500







dref <- lubridate::make_datetime(year=2026, month=1, day=29, hour=9, tz='UTC')
dref_num <- as.numeric(dref)
date_min <-dref_num-60*60
date_max <- dref_num+60*60*6

meteorologicos <- jsonlite::read_json(glue::glue('https://simcosta.furg.br/api/intrans_data?boiaID=515&type=json&time1={date_min}&time2={date_max}&params=Avg_Wnd_Dir_N,Gust_Sp,Avg_Dew,Avg_Air_Press,Avg_Sol_Rad,Avg_Air_Tmp,Avg_Hmt,Avg_Wnd_Sp'), simplifyVector = TRUE)

  ## C_Avg_Spd não está especificado no link api gerado
oceanograficos <- jsonlite::read_json(glue::glue('https://simcosta.furg.br/api/intrans_data?boiaID=515&type=json&time1={date_min}&time2={date_max}&params=H10,Havg,Hsig,HM0,Avg_Wv_Dir_N,Hmax,ZCN,Tp5,Tavg,T10,Tsig,Avg_Wv_Spread_N,Tp,Avg_Sal,Avg_W_Tmp1,Avg_W_Tmp2,Avg_CDOM,Avg_Chl,Avg_DO,Avg_Turb,C_Avg_Dir_N,tidbits_temp,C_Avg_Spd,C_Cell_2_North_Speed'), simplifyVector = TRUE)
#   #https://simcosta.furg.br/api/intrans_data?boiaID=515&type=json&time1=1740279600&time2=1740366000&params
correntes <- jsonlite::read_json(glue::glue('https://simcosta.furg.br/api/intrans_data?boiaID=515&type=json&time1={date_min}&time2={date_max}&params=perfil_correntes&extras=dir_n'), simplifyVector = TRUE)
boia_0 <- full_join(correntes, oceanograficos)%>%full_join(meteorologicos)
boia <- boia_0%>%
    transmute(#timestamp,
      date=lubridate::ymd_hms(timestamp, tz="UTC"),
      wind_direction=Avg_Wnd_Dir_N,
      wind_speed=Avg_Wnd_Sp,
      air_temperature=Avg_Air_Tmp,
      wave_height=Havg,
      wave_period=Tavg,
      wave_direction=Avg_Wv_Dir_N,
      ## oceanografica
      tide_speed_kmh=
        ## in milimiter/second
        ## so * 60 (minute) * 60 (hour)
        ## /1000 (meters)
        ## /1000 (km)
        C_Avg_Spd*60*60/1000/1000,
      tide_direction_1=as.numeric(C_Avg_Dir_N),
      ## correntes
      #tide_direction_1=as.numeric(`Avg_Cell(001)_dir_n`),
      tide_direction_2=as.numeric(`Avg_Cell(002)_dir_n`),
      tide_direction_avg=(tide_direction_1+tide_direction_2)/2)
boia_imp <- boia%>%
    ungroup%>%
    reframe(date=seq.POSIXt(from=min(date), to=max(date), by=60*10))%>%
    full_join(boia)%>%
    tidyr::pivot_longer(cols = -date)%>%
    arrange(name,date)%>%
    group_by(name)%>%
    mutate(ip_value=na.approx(value, na.rm=TRUE))%>%
    select(-value)%>%
    tidyr::pivot_wider(values_from ="ip_value")
}