# Metadaten SAB/SEB

Repository erstellt am: 07.10.2025
Release v0.1 am: 04.09.2026

**Kontakt**

- Christina Arn: <christina.arn@bi.zh.ch>
- Marco Schuppisser: <marco.schuppisser@zemces.ch>

## Kurzbeschreibung
Das R-Package [metaSABSEB](https://github.com/bildungsplanungZH/metaSABSEB) 
enthält **Metadaten** zum Qualitätsmonitoring Sek II. Es wurde durch die 
[Bildungsplanung des Kantons Zürich](https://www.zh.ch/de/bildungsdirektion/generalsekretariat-der-bildungsdirektion/bildungsplanung.html)
in Zusammenarbeit mit dem
[Schweizerischen Zentrum für die Mittelschule und für Schulevaluation auf Sekundarstufe ||](https://www.zemces.ch/de)
(ZEM CES) erarbeitet. 
Konkret geht es um Informationen zu folgenden Befragungen:

- Die standardisierte Abschlussklassenbefragung ([SAB](https://www.zemces.ch/de/evaluationen-und-befragungen/standardisierte-befragungen/abschlussklassenbefragung))
- Die standardisierte Ehemaligenbefragung ([SEB](https://www.zemces.ch/de/evaluationen-und-befragungen/standardisierte-befragungen/ehemaligenbefragung))

Die Metadaten beschreiben Spalten und Werteausprägungen und liefern wichtige 
Hintergrundinformationen zu den Daten.
Die gesammelten Metadaten beschränken sich auf die gesamtschweizerisch erhobenen 
Merkmale sowie auf Fokusmodule für den Kanton Zürich.
Metadaten zu den Fokusmodulen von anderen Kantonen wurden nicht berücksichtigt.


Darüber hinaus enthält das Package **Funktionen**, zum Abrufen, Anpassen und Ergänzen von Metadaten.

* `get_meta()`: Zum Abrufen von Metadaten
* `change_meta()`: Zum Ändern von einzelnen Metadatenfeldern von Variablen, die bereits erfasst wurden
* `add_meta()`: Zum Hinzufügen einer ganzen neuen Variable inkl. vorgeschriebener Metadatenfelder
* `delete_meta()`: Zum Löschen von Metadateneinträgen. Geeignet bei fälschlich hinzugefügten Informationen oder fürs Testen. Achtung die Funktion sollte nicht genuzt werden, um nicht mehr aktuelle Umfrageitems zu entfernen. Dafür kann das Metadatenfeld `status` der jeweiligen Variable aktualisiert werden.

Eine genaue Anleitung zur Nutzung der Funktionen findet sich in den Vignetten zum Paket. 

Installation des Pakets mit:
```
devtools::install_github("bildungsplanungZH/metaSABSEB", build_vignettes = T)
```


