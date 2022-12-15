## ABOUT
This builds an RShiny dashboard for working with data from [ActivityWatch]<https://activitywatch.net/>. Edit the app.r file to correctly point to the location of the SQLite Database. On Windows machines this is typically located in `C:/Users/USERNAME/AppData/Local/activitywatch/activitywatch/aw-server/peewee-sqlite.v2.db`.

Once the RShiny App is running, the dashboard can be accessed through a web browser via localhost:1993 (port can be changed by editing the primary file). 

The resulting dashboard will look as below:
![alt text](https://raw.githubusercontent.com/cmefi/aw-rshiny/master/images/ex1.png)