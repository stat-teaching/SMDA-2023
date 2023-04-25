# Lectures

lectures <- data.frame(
    stringsAsFactors = FALSE,
    Day = c("Wednesday","Thursday",
            "Tuesday","Wednesday","Thursday","Tuesday","Wednesday",
            "Thursday","Tuesday","Wednesday","Thursday"),
    Date = c("26/04/2023","27/04/2023",
             "02/05/2023","03/05/2023","04/05/2023","09/05/2023",
             "10/05/2023","11/05/2023","16/05/2023","17/05/2023",
             "18/05/2023"),
    Time = c("10:30-12:30","10:30-12:30",
             "10:30-12:30","10:30-12:30","10:30-12:30","10:30-12:30",
             "10:30-12:30","10:30-12:30","10:30-12:30",
             "10:30-12:30","10:30-12:30"),
    Room = c("3L","3L","3L","3L","3L",
             "3L","3L","3L","3L","3L","3L")
)

saveRDS(lectures, "objects/lectures.rds")

