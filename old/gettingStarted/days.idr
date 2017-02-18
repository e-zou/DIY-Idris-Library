data Day = Mon | Tues | Wed | Thurs | Fri | Sat | Sun

nextDay: Day -> Day
nd: Day-> Day
nd Mon = Tues
nd Tues = Wed
nd Wed = Thurs
nd Thurs = Fri
nd Fri = Sat
nd Sat = Sun
nd Sun = Mon
