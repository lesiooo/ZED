-   [Spis treści](#spis-treści)

### Spis treści

1.  [Podsumowanie analizy danych](#1)
2.  [Wykorzystane biblioteki](#2)
3.  [Wczytanie danych z pliku](#3)
4.  [Opis pliku wejściowego](#4)
5.  [Czyszczenie i przetwarzanie brakującyhc danych](#5)
6.  [Analiza atrybutów](#6)
7.  [Korelacja między atrybutami](#7)
8.  [Interaktywny wykres](#8)
9.  [Zapewnienie powtarzalności + Regresor](#9)
10. [Analiza ważności atrybutów](#10)

\#<a name="1"></a> \# Posumowanie analizy zbioru danych

Celem tego projektu jest prezentacja danych o przwidywanej długości
życia ludzi w poszczególnych krajach świata. Dane zbierane były w latach
2000 - 2015 przez przedstawicieli WHO (World Health Organization). Zbiór
danych zawiera m. in. dane o umieralności dzieci i dorosłych, odsetku
szczepień wykonywanych, czy danych ekonomicznych w poszczególnych
krajach.

Zbiór danych dostępny pod adresem: \#<a name="2"></a> \# Wykorzystane
biblioteki

``` r
library(plotly)
library(caret)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)
library(knitr)
library(tidyr)
```

\#<a name="3"></a> \# Wczytanie danych z pliku

Wczytanie danych z pliku. Za pomocą heurystyki wyznaczam typy danych.

``` r
file_data <- read.table("data.csv", sep = ',', comment.char = "", na.strings = 'NA', header = TRUE)
column_datatypes <- sapply(file_data, class)
raw_data <- read.table("data.csv", sep = ',', comment.char = "", na.strings = 'NA', header = TRUE, colClasses = column_datatypes)
```

Załadowano 2914 wierszy, które mają 22 kolumny.

\#<a name="4"></a> \# Opis pliku wejściowego

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 64%" />
</colgroup>
<thead>
<tr class="header">
<th>Atrybut</th>
<th>Opis</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Country</td>
<td>Kraj przeprowadzania badań</td>
</tr>
<tr class="even">
<td>Year</td>
<td>Rok w którym zbierano dane</td>
</tr>
<tr class="odd">
<td>Status</td>
<td>Oznaczenie kraju (rozwinięty lub rozwijający się)</td>
</tr>
<tr class="even">
<td>Life expectancy</td>
<td>Przewidywana średnia długość życia</td>
</tr>
<tr class="odd">
<td>Adult Mortality</td>
<td>wskaźnik umieralności u dorosłych, obojga płci między 15 a 60 rokiem życia na 1000 osób populacji</td>
</tr>
<tr class="even">
<td>infant deaths</td>
<td>liczba zgonów niemowląt na 1000 osób populacji</td>
</tr>
<tr class="odd">
<td>Alcohol</td>
<td>zarejestrowana średnia ilość w litrach czystego alkoholu</td>
</tr>
<tr class="even">
<td>percentage expenditure</td>
<td>wydatki na zdrowie, procent PKB</td>
</tr>
<tr class="odd">
<td>Hepatitis B</td>
<td>procentowa ochrona szczepienna przeciwko WZW B wśród dzieci w wieku 1 roku</td>
</tr>
<tr class="even">
<td>Measles</td>
<td>Liczba zgłoszonych zachorowań na Odrę na 1000 osób</td>
</tr>
<tr class="odd">
<td>BMI</td>
<td>średni wskaźnik BMI w całej populacji</td>
</tr>
<tr class="even">
<td>under-five deaths</td>
<td>liczba zgonów dzieci poniżej 5 roku życia na 1000 osób</td>
</tr>
<tr class="odd">
<td>Polio</td>
<td>Procent szczepień przeciwko Polio wśród 1-latków</td>
</tr>
<tr class="even">
<td>Total expenditure</td>
<td>Odsetek całkowitych wydatków na zdrowię przez instytucje rządowe i samorządowe</td>
</tr>
<tr class="odd">
<td>Diphtheria</td>
<td>Procent szczepień przeciwko krztuścowi wśród dzieci w wieku 1 roku</td>
</tr>
<tr class="even">
<td>HIV/AIDS</td>
<td>zgony na HIV/AIDS na 1000 osób populacji</td>
</tr>
<tr class="odd">
<td>GDP</td>
<td>PKB na mieszkańca w USD</td>
</tr>
<tr class="even">
<td>Population</td>
<td>Populacja kraju</td>
</tr>
<tr class="odd">
<td>thinness 1-19 years</td>
<td>Procent występowania niedowagi wśród dzieci w wieku 1-19 lat</td>
</tr>
<tr class="even">
<td>thinness 5-9 years</td>
<td>Procent występowania niedowagi wśród dzieci w wieku 5-9</td>
</tr>
<tr class="odd">
<td>Income composition of</td>
<td>Wskaźnik rozwoju społecznego pod względem struktury dochodów w zakresie zasobów (wskaźnik od 0 do 1)</td>
</tr>
<tr class="even">
<td>Schooling</td>
<td>Liczba lat nauki</td>
</tr>
</tbody>
</table>

\#<a name="5"></a> \# Czyszczenie danych, uzupełnienie brakujących
wartości

``` r
data_without_na_in_life_expectancy  <-  (raw_data[!is.na(raw_data$Life.expectancy),])
dim(data_without_na_in_life_expectancy)
for(i in 1:ncol(data_without_na_in_life_expectancy)){
  data_without_na_in_life_expectancy[is.na(data_without_na_in_life_expectancy[,i]), i] <- mean(as.numeric(data_without_na_in_life_expectancy[,i]), na.rm = TRUE)
}
clean_data <-data_without_na_in_life_expectancy
```

\#<a name="6"></a> \# Analiza atrybutów + rozkłady wartości atrybutów

``` r
summary(clean_data)
```

    ##                 Country          Year             Status     Life.expectancy
    ##  Afghanistan        :  16   Min.   :2000   Developed : 512   Min.   :36.30  
    ##  Albania            :  16   1st Qu.:2004   Developing:2392   1st Qu.:63.20  
    ##  Algeria            :  16   Median :2008                     Median :72.15  
    ##  Angola             :  16   Mean   :2007                     Mean   :69.29  
    ##  Antigua and Barbuda:  16   3rd Qu.:2011                     3rd Qu.:75.70  
    ##  Argentina          :  16   Max.   :2015                     Max.   :89.00  
    ##  (Other)            :2808                                                   
    ##  Adult.Mortality infant.deaths        Alcohol       percentage.expenditure
    ##  Min.   :  1.0   Min.   :   0.00   Min.   : 0.010   Min.   :    0.000     
    ##  1st Qu.: 73.0   1st Qu.:   0.00   1st Qu.: 1.097   1st Qu.:    5.593     
    ##  Median :143.0   Median :   3.00   Median : 4.185   Median :   67.721     
    ##  Mean   :163.9   Mean   :  30.44   Mean   : 4.625   Mean   :  746.440     
    ##  3rd Qu.:227.0   3rd Qu.:  22.00   3rd Qu.: 7.442   3rd Qu.:  448.638     
    ##  Max.   :723.0   Max.   :1800.00   Max.   :17.870   Max.   :19479.912     
    ##                                                                           
    ##   Hepatitis.B       Measles              BMI        under.five.deaths
    ##  Min.   : 2.00   Min.   :     0.0   Min.   : 1.00   Min.   :   0.00  
    ##  1st Qu.:81.12   1st Qu.:     0.0   1st Qu.:19.48   1st Qu.:   0.00  
    ##  Median :87.00   Median :    17.0   Median :43.20   Median :   4.00  
    ##  Mean   :81.12   Mean   :  2441.3   Mean   :38.38   Mean   :  42.22  
    ##  3rd Qu.:96.00   3rd Qu.:   362.2   3rd Qu.:56.10   3rd Qu.:  28.00  
    ##  Max.   :99.00   Max.   :212183.0   Max.   :77.60   Max.   :2500.00  
    ##                                                                      
    ##      Polio       Total.expenditure   Diphtheria       HIV.AIDS     
    ##  Min.   : 3.00   Min.   : 0.370    Min.   : 2.00   Min.   : 0.100  
    ##  1st Qu.:78.00   1st Qu.: 4.370    1st Qu.:78.00   1st Qu.: 0.100  
    ##  Median :93.00   Median : 5.937    Median :93.00   Median : 0.100  
    ##  Mean   :82.61   Mean   : 5.937    Mean   :82.43   Mean   : 1.749  
    ##  3rd Qu.:97.00   3rd Qu.: 7.343    3rd Qu.:97.00   3rd Qu.: 0.800  
    ##  Max.   :99.00   Max.   :17.600    Max.   :99.00   Max.   :50.600  
    ##                                                                    
    ##       GDP              Population        thinness..1.19.years
    ##  Min.   :     1.68   Min.   :3.400e+01   Min.   : 0.10       
    ##  1st Qu.:   571.43   1st Qu.:3.983e+05   1st Qu.: 1.60       
    ##  Median :  2965.48   Median :3.515e+06   Median : 3.40       
    ##  Mean   :  7494.21   Mean   :1.276e+07   Mean   : 4.84       
    ##  3rd Qu.:  7494.21   3rd Qu.:1.276e+07   3rd Qu.: 7.10       
    ##  Max.   :119172.74   Max.   :1.294e+09   Max.   :27.70       
    ##                                                              
    ##  thinness.5.9.years Income.composition.of.resources   Schooling    
    ##  Min.   : 0.100     Min.   :0.0000                  Min.   : 0.00  
    ##  1st Qu.: 1.600     1st Qu.:0.5028                  1st Qu.:10.30  
    ##  Median : 3.400     Median :0.6650                  Median :12.20  
    ##  Mean   : 4.871     Mean   :0.6277                  Mean   :12.01  
    ##  3rd Qu.: 7.200     3rd Qu.:0.7730                  3rd Qu.:14.20  
    ##  Max.   :28.600     Max.   :0.9480                  Max.   :20.70  
    ## 

``` r
summary(raw_data)
```

    ##                 Country          Year             Status     Life.expectancy
    ##  Afghanistan        :  16   Min.   :2000   Developed : 512   Min.   :36.30  
    ##  Albania            :  16   1st Qu.:2004   Developing:2402   1st Qu.:63.20  
    ##  Algeria            :  16   Median :2008                     Median :72.15  
    ##  Angola             :  16   Mean   :2008                     Mean   :69.29  
    ##  Antigua and Barbuda:  16   3rd Qu.:2012                     3rd Qu.:75.70  
    ##  Argentina          :  16   Max.   :2015                     Max.   :89.00  
    ##  (Other)            :2818                                    NA's   :10     
    ##  Adult.Mortality infant.deaths        Alcohol       percentage.expenditure
    ##  Min.   :  1.0   Min.   :   0.00   Min.   : 0.010   Min.   :    0.00      
    ##  1st Qu.: 73.0   1st Qu.:   0.00   1st Qu.: 0.860   1st Qu.:    5.45      
    ##  Median :143.0   Median :   3.00   Median : 3.790   Median :   67.69      
    ##  Mean   :163.9   Mean   :  30.33   Mean   : 4.613   Mean   :  744.33      
    ##  3rd Qu.:227.0   3rd Qu.:  22.00   3rd Qu.: 7.755   3rd Qu.:  447.50      
    ##  Max.   :723.0   Max.   :1800.00   Max.   :17.870   Max.   :19479.91      
    ##  NA's   :10                        NA's   :191                            
    ##   Hepatitis.B       Measles              BMI        under.five.deaths
    ##  Min.   : 2.00   Min.   :     0.0   Min.   : 1.00   Min.   :   0.00  
    ##  1st Qu.:77.00   1st Qu.:     0.0   1st Qu.:19.38   1st Qu.:   0.00  
    ##  Median :92.00   Median :    16.0   Median :43.80   Median :   4.00  
    ##  Mean   :81.09   Mean   :  2432.9   Mean   :38.47   Mean   :  42.07  
    ##  3rd Qu.:97.00   3rd Qu.:   360.2   3rd Qu.:56.30   3rd Qu.:  27.00  
    ##  Max.   :99.00   Max.   :212183.0   Max.   :87.30   Max.   :2500.00  
    ##  NA's   :551                        NA's   :34                       
    ##      Polio       Total.expenditure   Diphtheria       HIV.AIDS     
    ##  Min.   : 3.00   Min.   : 0.370    Min.   : 2.00   Min.   : 0.100  
    ##  1st Qu.:78.00   1st Qu.: 4.260    1st Qu.:78.00   1st Qu.: 0.100  
    ##  Median :93.00   Median : 5.760    Median :93.00   Median : 0.100  
    ##  Mean   :82.61   Mean   : 5.945    Mean   :82.43   Mean   : 1.743  
    ##  3rd Qu.:97.00   3rd Qu.: 7.500    3rd Qu.:97.00   3rd Qu.: 0.800  
    ##  Max.   :99.00   Max.   :17.600    Max.   :99.00   Max.   :50.600  
    ##  NA's   :19      NA's   :216       NA's   :19                      
    ##       GDP              Population        thinness..1.19.years
    ##  Min.   :     1.68   Min.   :3.400e+01   Min.   : 0.100      
    ##  1st Qu.:   463.94   1st Qu.:1.958e+05   1st Qu.: 1.600      
    ##  Median :  1766.95   Median :1.387e+06   Median : 3.300      
    ##  Mean   :  7483.16   Mean   :1.275e+07   Mean   : 4.829      
    ##  3rd Qu.:  5910.81   3rd Qu.:7.420e+06   3rd Qu.: 7.200      
    ##  Max.   :119172.74   Max.   :1.294e+09   Max.   :27.700      
    ##  NA's   :424         NA's   :628         NA's   :34          
    ##  thinness.5.9.years Income.composition.of.resources   Schooling   
    ##  Min.   : 0.10      Min.   :0.0000                  Min.   : 0.0  
    ##  1st Qu.: 1.50      1st Qu.:0.4930                  1st Qu.:10.1  
    ##  Median : 3.30      Median :0.6780                  Median :12.4  
    ##  Mean   : 4.86      Mean   :0.6279                  Mean   :12.0  
    ##  3rd Qu.: 7.20      3rd Qu.:0.7800                  3rd Qu.:14.3  
    ##  Max.   :28.60      Max.   :0.9480                  Max.   :20.7  
    ##  NA's   :34         NA's   :151                     NA's   :147

``` r
attr<-select(data_without_na_in_life_expectancy,-Country, -Year, -Status)
a <- list()
for (col in colnames(attr)) {
a[[col]] <-((ggplot(attr) +
    aes_string(x=col) +
    geom_histogram(bins = 30))) 
}

do.call("grid.arrange",c(a, ncol = 5))
```

![](final_files/figure-markdown_github/atrybuty-1.png)

\#<a name="7"></a> \# Korelacja między atrybutami

Analizę związków pomiędzy atrybutami dokonano za pomocą wizualizacji
macierzy korelacji.

``` r
corMatrix <- cor(data_without_na_in_life_expectancy %>% select(-Country, -Status),use="complete.obs")
#clases
corrplot(corMatrix, method = "square")
```

![](final_files/figure-markdown_github/corAnalysis-1.png)

\#<a name="8"></a> \# Interaktywny wykres

``` r
wykres <- data_without_na_in_life_expectancy[1:151, ] %>% group_by(Country, Year) %>% summarise(life = Life.expectancy)

ggplotwykres <- ggplot(data = wykres, aes(wykres$Country
, wykres$life,  color=factor(wykres$life))) + 
  geom_point()+
  labs(y="Year") + 
  labs(x="Country")+
  labs("Interaktywny wykres")
ggplotly(ggplotwykres,dynamicTicks = TRUE,width = NULL, height = NULL)
```

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-ed8b6d8124cc36e19564">{"x":{"data":[{"x":["Angola"],"y":[45.3],"text":"wykres$Country: Angola<br />wykres$life: 45.3<br />factor(wykres$life): 45.3","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"45.3","legendgroup":"45.3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[45.7],"text":"wykres$Country: Angola<br />wykres$life: 45.7<br />factor(wykres$life): 45.7","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(246,121,100,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(246,121,100,1)"}},"hoveron":"points","name":"45.7","legendgroup":"45.7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[46.5],"text":"wykres$Country: Angola<br />wykres$life: 46.5<br />factor(wykres$life): 46.5","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(243,123,90,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(243,123,90,1)"}},"hoveron":"points","name":"46.5","legendgroup":"46.5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[46.8],"text":"wykres$Country: Angola<br />wykres$life: 46.8<br />factor(wykres$life): 46.8","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(241,126,79,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(241,126,79,1)"}},"hoveron":"points","name":"46.8","legendgroup":"46.8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[47.1],"text":"wykres$Country: Angola<br />wykres$life: 47.1<br />factor(wykres$life): 47.1","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(238,128,68,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(238,128,68,1)"}},"hoveron":"points","name":"47.1","legendgroup":"47.1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[47.4],"text":"wykres$Country: Angola<br />wykres$life: 47.4<br />factor(wykres$life): 47.4","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(235,130,54,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(235,130,54,1)"}},"hoveron":"points","name":"47.4","legendgroup":"47.4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[47.7],"text":"wykres$Country: Angola<br />wykres$life: 47.7<br />factor(wykres$life): 47.7","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(232,133,36,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(232,133,36,1)"}},"hoveron":"points","name":"47.7","legendgroup":"47.7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[48.2],"text":"wykres$Country: Angola<br />wykres$life: 48.2<br />factor(wykres$life): 48.2","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(229,135,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(229,135,0,1)"}},"hoveron":"points","name":"48.2","legendgroup":"48.2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[48.7],"text":"wykres$Country: Angola<br />wykres$life: 48.7<br />factor(wykres$life): 48.7","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(225,138,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(225,138,0,1)"}},"hoveron":"points","name":"48.7","legendgroup":"48.7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[49.1],"text":"wykres$Country: Angola<br />wykres$life: 49.1<br />factor(wykres$life): 49.1","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(222,140,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(222,140,0,1)"}},"hoveron":"points","name":"49.1","legendgroup":"49.1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[49.6],"text":"wykres$Country: Angola<br />wykres$life: 49.6<br />factor(wykres$life): 49.6","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(218,142,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(218,142,0,1)"}},"hoveron":"points","name":"49.6","legendgroup":"49.6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[51],"text":"wykres$Country: Angola<br />wykres$life: 51.0<br />factor(wykres$life): 51","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(214,145,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(214,145,0,1)"}},"hoveron":"points","name":"51","legendgroup":"51","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[51.1],"text":"wykres$Country: Angola<br />wykres$life: 51.1<br />factor(wykres$life): 51.1","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(210,147,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(210,147,0,1)"}},"hoveron":"points","name":"51.1","legendgroup":"51.1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[51.7],"text":"wykres$Country: Angola<br />wykres$life: 51.7<br />factor(wykres$life): 51.7","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(206,149,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(206,149,0,1)"}},"hoveron":"points","name":"51.7","legendgroup":"51.7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[52.4],"text":"wykres$Country: Angola<br />wykres$life: 52.4<br />factor(wykres$life): 52.4","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(201,151,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(201,151,0,1)"}},"hoveron":"points","name":"52.4","legendgroup":"52.4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan"],"y":[54.8],"text":"wykres$Country: Afghanistan<br />wykres$life: 54.8<br />factor(wykres$life): 54.8","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(197,153,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(197,153,0,1)"}},"hoveron":"points","name":"54.8","legendgroup":"54.8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan"],"y":[55.3],"text":"wykres$Country: Afghanistan<br />wykres$life: 55.3<br />factor(wykres$life): 55.3","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(192,155,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(192,155,0,1)"}},"hoveron":"points","name":"55.3","legendgroup":"55.3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Angola"],"y":[56],"text":"wykres$Country: Angola<br />wykres$life: 56.0<br />factor(wykres$life): 56","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(187,157,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(187,157,0,1)"}},"hoveron":"points","name":"56","legendgroup":"56","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan"],"y":[56.2],"text":"wykres$Country: Afghanistan<br />wykres$life: 56.2<br />factor(wykres$life): 56.2","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(181,159,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(181,159,0,1)"}},"hoveron":"points","name":"56.2","legendgroup":"56.2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan"],"y":[56.7],"text":"wykres$Country: Afghanistan<br />wykres$life: 56.7<br />factor(wykres$life): 56.7","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(176,161,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(176,161,0,1)"}},"hoveron":"points","name":"56.7","legendgroup":"56.7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan"],"y":[57],"text":"wykres$Country: Afghanistan<br />wykres$life: 57.0<br />factor(wykres$life): 57","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(170,163,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(170,163,0,1)"}},"hoveron":"points","name":"57","legendgroup":"57","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan","Afghanistan"],"y":[57.3,57.3],"text":"wykres$Country: Afghanistan<br />wykres$life: 57.3<br />factor(wykres$life): 57.3","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(164,165,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(164,165,0,1)"}},"hoveron":"points","name":"57.3","legendgroup":"57.3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan"],"y":[57.5],"text":"wykres$Country: Afghanistan<br />wykres$life: 57.5<br />factor(wykres$life): 57.5","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(158,167,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(158,167,0,1)"}},"hoveron":"points","name":"57.5","legendgroup":"57.5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan"],"y":[58.1],"text":"wykres$Country: Afghanistan<br />wykres$life: 58.1<br />factor(wykres$life): 58.1","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(151,169,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(151,169,0,1)"}},"hoveron":"points","name":"58.1","legendgroup":"58.1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan"],"y":[58.6],"text":"wykres$Country: Afghanistan<br />wykres$life: 58.6<br />factor(wykres$life): 58.6","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(144,170,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(144,170,0,1)"}},"hoveron":"points","name":"58.6","legendgroup":"58.6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan"],"y":[58.8],"text":"wykres$Country: Afghanistan<br />wykres$life: 58.8<br />factor(wykres$life): 58.8","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(136,172,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(136,172,0,1)"}},"hoveron":"points","name":"58.8","legendgroup":"58.8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan"],"y":[59.2],"text":"wykres$Country: Afghanistan<br />wykres$life: 59.2<br />factor(wykres$life): 59.2","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(128,173,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(128,173,0,1)"}},"hoveron":"points","name":"59.2","legendgroup":"59.2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan"],"y":[59.5],"text":"wykres$Country: Afghanistan<br />wykres$life: 59.5<br />factor(wykres$life): 59.5","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(119,175,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(119,175,0,1)"}},"hoveron":"points","name":"59.5","legendgroup":"59.5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan","Afghanistan"],"y":[59.9,59.9],"text":"wykres$Country: Afghanistan<br />wykres$life: 59.9<br />factor(wykres$life): 59.9","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(110,177,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(110,177,0,1)"}},"hoveron":"points","name":"59.9","legendgroup":"59.9","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Afghanistan"],"y":[65],"text":"wykres$Country: Afghanistan<br />wykres$life: 65.0<br />factor(wykres$life): 65","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(99,178,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(99,178,0,1)"}},"hoveron":"points","name":"65","legendgroup":"65","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Azerbaijan"],"y":[71.1],"text":"wykres$Country: Azerbaijan<br />wykres$life: 71.1<br />factor(wykres$life): 71.1","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(87,179,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(87,179,0,1)"}},"hoveron":"points","name":"71.1","legendgroup":"71.1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria"],"y":[71.3],"text":"wykres$Country: Algeria<br />wykres$life: 71.3<br />factor(wykres$life): 71.3","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(72,181,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(72,181,0,1)"}},"hoveron":"points","name":"71.3","legendgroup":"71.3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria"],"y":[71.4],"text":"wykres$Country: Algeria<br />wykres$life: 71.4<br />factor(wykres$life): 71.4","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(53,182,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(53,182,0,1)"}},"hoveron":"points","name":"71.4","legendgroup":"71.4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria","Azerbaijan"],"y":[71.6,71.6],"text":["wykres$Country: Algeria<br />wykres$life: 71.6<br />factor(wykres$life): 71.6","wykres$Country: Azerbaijan<br />wykres$life: 71.6<br />factor(wykres$life): 71.6"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(20,183,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(20,183,0,1)"}},"hoveron":"points","name":"71.6","legendgroup":"71.6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria"],"y":[71.7],"text":"wykres$Country: Algeria<br />wykres$life: 71.7<br />factor(wykres$life): 71.7","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,184,33,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,184,33,1)"}},"hoveron":"points","name":"71.7","legendgroup":"71.7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Azerbaijan"],"y":[71.9],"text":"wykres$Country: Azerbaijan<br />wykres$life: 71.9<br />factor(wykres$life): 71.9","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,185,51,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,185,51,1)"}},"hoveron":"points","name":"71.9","legendgroup":"71.9","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Armenia"],"y":[72],"text":"wykres$Country: Armenia<br />wykres$life: 72.0<br />factor(wykres$life): 72","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,186,65,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,186,65,1)"}},"hoveron":"points","name":"72","legendgroup":"72","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Azerbaijan"],"y":[72.2],"text":"wykres$Country: Azerbaijan<br />wykres$life: 72.2<br />factor(wykres$life): 72.2","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,187,77,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,187,77,1)"}},"hoveron":"points","name":"72.2","legendgroup":"72.2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria"],"y":[72.3],"text":"wykres$Country: Algeria<br />wykres$life: 72.3<br />factor(wykres$life): 72.3","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,188,87,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,188,87,1)"}},"hoveron":"points","name":"72.3","legendgroup":"72.3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Azerbaijan"],"y":[72.5],"text":"wykres$Country: Azerbaijan<br />wykres$life: 72.5<br />factor(wykres$life): 72.5","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,189,97,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,189,97,1)"}},"hoveron":"points","name":"72.5","legendgroup":"72.5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania","Armenia","Armenia"],"y":[72.6,72.6,72.6],"text":["wykres$Country: Albania<br />wykres$life: 72.6<br />factor(wykres$life): 72.6","wykres$Country: Armenia<br />wykres$life: 72.6<br />factor(wykres$life): 72.6","wykres$Country: Armenia<br />wykres$life: 72.6<br />factor(wykres$life): 72.6"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,190,106,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,190,106,1)"}},"hoveron":"points","name":"72.6","legendgroup":"72.6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Armenia","Azerbaijan"],"y":[72.7,72.7],"text":["wykres$Country: Armenia<br />wykres$life: 72.7<br />factor(wykres$life): 72.7","wykres$Country: Azerbaijan<br />wykres$life: 72.7<br />factor(wykres$life): 72.7"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,190,114,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,190,114,1)"}},"hoveron":"points","name":"72.7","legendgroup":"72.7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania"],"y":[72.8],"text":"wykres$Country: Albania<br />wykres$life: 72.8<br />factor(wykres$life): 72.8","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,191,122,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,122,1)"}},"hoveron":"points","name":"72.8","legendgroup":"72.8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria","Armenia"],"y":[72.9,72.9],"text":["wykres$Country: Algeria<br />wykres$life: 72.9<br />factor(wykres$life): 72.9","wykres$Country: Armenia<br />wykres$life: 72.9<br />factor(wykres$life): 72.9"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,192,130,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,192,130,1)"}},"hoveron":"points","name":"72.9","legendgroup":"72.9","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania","Armenia","Armenia"],"y":[73,73,73],"text":["wykres$Country: Albania<br />wykres$life: 73.0<br />factor(wykres$life): 73","wykres$Country: Armenia<br />wykres$life: 73.0<br />factor(wykres$life): 73","wykres$Country: Armenia<br />wykres$life: 73.0<br />factor(wykres$life): 73"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,192,137,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,192,137,1)"}},"hoveron":"points","name":"73","legendgroup":"73","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Armenia"],"y":[73.2],"text":"wykres$Country: Armenia<br />wykres$life: 73.2<br />factor(wykres$life): 73.2","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,192,145,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,192,145,1)"}},"hoveron":"points","name":"73.2","legendgroup":"73.2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania","Armenia"],"y":[73.3,73.3],"text":["wykres$Country: Albania<br />wykres$life: 73.3<br />factor(wykres$life): 73.3","wykres$Country: Armenia<br />wykres$life: 73.3<br />factor(wykres$life): 73.3"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,192,152,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,192,152,1)"}},"hoveron":"points","name":"73.3","legendgroup":"73.3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria"],"y":[73.4],"text":"wykres$Country: Algeria<br />wykres$life: 73.4<br />factor(wykres$life): 73.4","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,193,159,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,193,159,1)"}},"hoveron":"points","name":"73.4","legendgroup":"73.4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania","Armenia","Armenia"],"y":[73.5,73.5,73.5],"text":["wykres$Country: Albania<br />wykres$life: 73.5<br />factor(wykres$life): 73.5","wykres$Country: Armenia<br />wykres$life: 73.5<br />factor(wykres$life): 73.5","wykres$Country: Armenia<br />wykres$life: 73.5<br />factor(wykres$life): 73.5"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,193,165,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,193,165,1)"}},"hoveron":"points","name":"73.5","legendgroup":"73.5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania","Antigua and Barbuda"],"y":[73.6,73.6],"text":["wykres$Country: Albania<br />wykres$life: 73.6<br />factor(wykres$life): 73.6","wykres$Country: Antigua and Barbuda<br />wykres$life: 73.6<br />factor(wykres$life): 73.6"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,193,172,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,193,172,1)"}},"hoveron":"points","name":"73.6","legendgroup":"73.6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria","Antigua and Barbuda"],"y":[73.8,73.8],"text":["wykres$Country: Algeria<br />wykres$life: 73.8<br />factor(wykres$life): 73.8","wykres$Country: Antigua and Barbuda<br />wykres$life: 73.8<br />factor(wykres$life): 73.8"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,192,178,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,192,178,1)"}},"hoveron":"points","name":"73.8","legendgroup":"73.8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Armenia"],"y":[73.9],"text":"wykres$Country: Armenia<br />wykres$life: 73.9<br />factor(wykres$life): 73.9","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,192,184,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,192,184,1)"}},"hoveron":"points","name":"73.9","legendgroup":"73.9","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Antigua and Barbuda","Argentina"],"y":[74,74],"text":["wykres$Country: Antigua and Barbuda<br />wykres$life: 74.0<br />factor(wykres$life): 74","wykres$Country: Argentina<br />wykres$life: 74.0<br />factor(wykres$life): 74"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,192,191,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,192,191,1)"}},"hoveron":"points","name":"74","legendgroup":"74","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria","Argentina","Argentina","Argentina"],"y":[74.1,74.1,74.1,74.1],"text":["wykres$Country: Algeria<br />wykres$life: 74.1<br />factor(wykres$life): 74.1","wykres$Country: Argentina<br />wykres$life: 74.1<br />factor(wykres$life): 74.1","wykres$Country: Argentina<br />wykres$life: 74.1<br />factor(wykres$life): 74.1","wykres$Country: Argentina<br />wykres$life: 74.1<br />factor(wykres$life): 74.1"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"74.1","legendgroup":"74.1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania","Antigua and Barbuda"],"y":[74.2,74.2],"text":["wykres$Country: Albania<br />wykres$life: 74.2<br />factor(wykres$life): 74.2","wykres$Country: Antigua and Barbuda<br />wykres$life: 74.2<br />factor(wykres$life): 74.2"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,190,202,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,190,202,1)"}},"hoveron":"points","name":"74.2","legendgroup":"74.2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria","Antigua and Barbuda","Armenia","Armenia"],"y":[74.4,74.4,74.4,74.4],"text":["wykres$Country: Algeria<br />wykres$life: 74.4<br />factor(wykres$life): 74.4","wykres$Country: Antigua and Barbuda<br />wykres$life: 74.4<br />factor(wykres$life): 74.4","wykres$Country: Armenia<br />wykres$life: 74.4<br />factor(wykres$life): 74.4","wykres$Country: Armenia<br />wykres$life: 74.4<br />factor(wykres$life): 74.4"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,190,208,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,190,208,1)"}},"hoveron":"points","name":"74.4","legendgroup":"74.4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Antigua and Barbuda","Armenia"],"y":[74.6,74.6],"text":["wykres$Country: Antigua and Barbuda<br />wykres$life: 74.6<br />factor(wykres$life): 74.6","wykres$Country: Armenia<br />wykres$life: 74.6<br />factor(wykres$life): 74.6"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,189,213,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,189,213,1)"}},"hoveron":"points","name":"74.6","legendgroup":"74.6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria","Argentina"],"y":[74.7,74.7],"text":["wykres$Country: Algeria<br />wykres$life: 74.7<br />factor(wykres$life): 74.7","wykres$Country: Argentina<br />wykres$life: 74.7<br />factor(wykres$life): 74.7"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,187,218,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,187,218,1)"}},"hoveron":"points","name":"74.7","legendgroup":"74.7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Antigua and Barbuda","Argentina","Armenia"],"y":[74.8,74.8,74.8],"text":["wykres$Country: Antigua and Barbuda<br />wykres$life: 74.8<br />factor(wykres$life): 74.8","wykres$Country: Argentina<br />wykres$life: 74.8<br />factor(wykres$life): 74.8","wykres$Country: Armenia<br />wykres$life: 74.8<br />factor(wykres$life): 74.8"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,186,223,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,186,223,1)"}},"hoveron":"points","name":"74.8","legendgroup":"74.8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria","Argentina"],"y":[74.9,74.9],"text":["wykres$Country: Algeria<br />wykres$life: 74.9<br />factor(wykres$life): 74.9","wykres$Country: Argentina<br />wykres$life: 74.9<br />factor(wykres$life): 74.9"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,185,228,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,185,228,1)"}},"hoveron":"points","name":"74.9","legendgroup":"74.9","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Antigua and Barbuda"],"y":[75],"text":"wykres$Country: Antigua and Barbuda<br />wykres$life: 75.0<br />factor(wykres$life): 75","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,183,232,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,183,232,1)"}},"hoveron":"points","name":"75","legendgroup":"75","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria"],"y":[75.1],"text":"wykres$Country: Algeria<br />wykres$life: 75.1<br />factor(wykres$life): 75.1","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,181,237,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,181,237,1)"}},"hoveron":"points","name":"75.1","legendgroup":"75.1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Antigua and Barbuda","Argentina"],"y":[75.2,75.2],"text":["wykres$Country: Antigua and Barbuda<br />wykres$life: 75.2<br />factor(wykres$life): 75.2","wykres$Country: Argentina<br />wykres$life: 75.2<br />factor(wykres$life): 75.2"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,179,241,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,179,241,1)"}},"hoveron":"points","name":"75.2","legendgroup":"75.2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania","Algeria"],"y":[75.3,75.3],"text":["wykres$Country: Albania<br />wykres$life: 75.3<br />factor(wykres$life): 75.3","wykres$Country: Algeria<br />wykres$life: 75.3<br />factor(wykres$life): 75.3"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,177,244,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,177,244,1)"}},"hoveron":"points","name":"75.3","legendgroup":"75.3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria","Antigua and Barbuda","Argentina"],"y":[75.4,75.4,75.4],"text":["wykres$Country: Algeria<br />wykres$life: 75.4<br />factor(wykres$life): 75.4","wykres$Country: Antigua and Barbuda<br />wykres$life: 75.4<br />factor(wykres$life): 75.4","wykres$Country: Argentina<br />wykres$life: 75.4<br />factor(wykres$life): 75.4"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,175,248,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,175,248,1)"}},"hoveron":"points","name":"75.4","legendgroup":"75.4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Argentina"],"y":[75.5],"text":"wykres$Country: Argentina<br />wykres$life: 75.5<br />factor(wykres$life): 75.5","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,173,251,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,173,251,1)"}},"hoveron":"points","name":"75.5","legendgroup":"75.5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Algeria","Antigua and Barbuda","Argentina"],"y":[75.6,75.6,75.6],"text":["wykres$Country: Algeria<br />wykres$life: 75.6<br />factor(wykres$life): 75.6","wykres$Country: Antigua and Barbuda<br />wykres$life: 75.6<br />factor(wykres$life): 75.6","wykres$Country: Argentina<br />wykres$life: 75.6<br />factor(wykres$life): 75.6"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,170,254,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,170,254,1)"}},"hoveron":"points","name":"75.6","legendgroup":"75.6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Antigua and Barbuda","Argentina"],"y":[75.7,75.7],"text":["wykres$Country: Antigua and Barbuda<br />wykres$life: 75.7<br />factor(wykres$life): 75.7","wykres$Country: Argentina<br />wykres$life: 75.7<br />factor(wykres$life): 75.7"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,167,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,167,255,1)"}},"hoveron":"points","name":"75.7","legendgroup":"75.7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania","Antigua and Barbuda","Argentina"],"y":[75.9,75.9,75.9],"text":["wykres$Country: Albania<br />wykres$life: 75.9<br />factor(wykres$life): 75.9","wykres$Country: Antigua and Barbuda<br />wykres$life: 75.9<br />factor(wykres$life): 75.9","wykres$Country: Argentina<br />wykres$life: 75.9<br />factor(wykres$life): 75.9"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,164,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,164,255,1)"}},"hoveron":"points","name":"75.9","legendgroup":"75.9","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Argentina"],"y":[76],"text":"wykres$Country: Argentina<br />wykres$life: 76.0<br />factor(wykres$life): 76","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(57,161,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(57,161,255,1)"}},"hoveron":"points","name":"76","legendgroup":"76","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania","Antigua and Barbuda"],"y":[76.1,76.1],"text":["wykres$Country: Albania<br />wykres$life: 76.1<br />factor(wykres$life): 76.1","wykres$Country: Antigua and Barbuda<br />wykres$life: 76.1<br />factor(wykres$life): 76.1"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(83,158,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(83,158,255,1)"}},"hoveron":"points","name":"76.1","legendgroup":"76.1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania","Antigua and Barbuda","Argentina"],"y":[76.2,76.2,76.2],"text":["wykres$Country: Albania<br />wykres$life: 76.2<br />factor(wykres$life): 76.2","wykres$Country: Antigua and Barbuda<br />wykres$life: 76.2<br />factor(wykres$life): 76.2","wykres$Country: Argentina<br />wykres$life: 76.2<br />factor(wykres$life): 76.2"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(103,155,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(103,155,255,1)"}},"hoveron":"points","name":"76.2","legendgroup":"76.2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Argentina"],"y":[76.3],"text":"wykres$Country: Argentina<br />wykres$life: 76.3<br />factor(wykres$life): 76.3","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(119,151,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(119,151,255,1)"}},"hoveron":"points","name":"76.3","legendgroup":"76.3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Antigua and Barbuda"],"y":[76.4],"text":"wykres$Country: Antigua and Barbuda<br />wykres$life: 76.4<br />factor(wykres$life): 76.4","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(134,148,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(134,148,255,1)"}},"hoveron":"points","name":"76.4","legendgroup":"76.4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania"],"y":[76.6],"text":"wykres$Country: Albania<br />wykres$life: 76.6<br />factor(wykres$life): 76.6","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(146,144,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(146,144,255,1)"}},"hoveron":"points","name":"76.6","legendgroup":"76.6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania"],"y":[76.9],"text":"wykres$Country: Albania<br />wykres$life: 76.9<br />factor(wykres$life): 76.9","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(158,141,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(158,141,255,1)"}},"hoveron":"points","name":"76.9","legendgroup":"76.9","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania"],"y":[77.2],"text":"wykres$Country: Albania<br />wykres$life: 77.2<br />factor(wykres$life): 77.2","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(168,137,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(168,137,255,1)"}},"hoveron":"points","name":"77.2","legendgroup":"77.2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania"],"y":[77.5],"text":"wykres$Country: Albania<br />wykres$life: 77.5<br />factor(wykres$life): 77.5","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(178,133,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(178,133,255,1)"}},"hoveron":"points","name":"77.5","legendgroup":"77.5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Albania"],"y":[77.8],"text":"wykres$Country: Albania<br />wykres$life: 77.8<br />factor(wykres$life): 77.8","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(187,130,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(187,130,255,1)"}},"hoveron":"points","name":"77.8","legendgroup":"77.8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Azerbaijan"],"y":[78],"text":"wykres$Country: Azerbaijan<br />wykres$life: 78.0<br />factor(wykres$life): 78","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(195,126,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(195,126,255,1)"}},"hoveron":"points","name":"78","legendgroup":"78","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Austria"],"y":[78.1],"text":"wykres$Country: Austria<br />wykres$life: 78.1<br />factor(wykres$life): 78.1","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(203,122,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(203,122,255,1)"}},"hoveron":"points","name":"78.1","legendgroup":"78.1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Austria"],"y":[78.6],"text":"wykres$Country: Austria<br />wykres$life: 78.6<br />factor(wykres$life): 78.6","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(210,119,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(210,119,255,1)"}},"hoveron":"points","name":"78.6","legendgroup":"78.6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Austria"],"y":[78.7],"text":"wykres$Country: Austria<br />wykres$life: 78.7<br />factor(wykres$life): 78.7","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(216,116,252,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(216,116,252,1)"}},"hoveron":"points","name":"78.7","legendgroup":"78.7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Austria"],"y":[78.8],"text":"wykres$Country: Austria<br />wykres$life: 78.8<br />factor(wykres$life): 78.8","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(222,112,249,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(222,112,249,1)"}},"hoveron":"points","name":"78.8","legendgroup":"78.8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Austria"],"y":[79.3],"text":"wykres$Country: Austria<br />wykres$life: 79.3<br />factor(wykres$life): 79.3","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(227,109,246,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(227,109,246,1)"}},"hoveron":"points","name":"79.3","legendgroup":"79.3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Austria"],"y":[79.4],"text":"wykres$Country: Austria<br />wykres$life: 79.4<br />factor(wykres$life): 79.4","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(232,107,242,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(232,107,242,1)"}},"hoveron":"points","name":"79.4","legendgroup":"79.4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia"],"y":[79.5],"text":"wykres$Country: Australia<br />wykres$life: 79.5<br />factor(wykres$life): 79.5","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(236,104,238,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(236,104,238,1)"}},"hoveron":"points","name":"79.5","legendgroup":"79.5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Austria"],"y":[79.8],"text":"wykres$Country: Austria<br />wykres$life: 79.8<br />factor(wykres$life): 79.8","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(240,102,234,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(240,102,234,1)"}},"hoveron":"points","name":"79.8","legendgroup":"79.8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia","Australia"],"y":[79.9,79.9],"text":"wykres$Country: Australia<br />wykres$life: 79.9<br />factor(wykres$life): 79.9","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(244,100,229,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(244,100,229,1)"}},"hoveron":"points","name":"79.9","legendgroup":"79.9","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia","Austria"],"y":[81,81],"text":["wykres$Country: Australia<br />wykres$life: 81.0<br />factor(wykres$life): 81","wykres$Country: Austria<br />wykres$life: 81.0<br />factor(wykres$life): 81"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(247,99,224,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(247,99,224,1)"}},"hoveron":"points","name":"81","legendgroup":"81","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Austria"],"y":[81.1],"text":"wykres$Country: Austria<br />wykres$life: 81.1<br />factor(wykres$life): 81.1","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(249,98,219,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(249,98,219,1)"}},"hoveron":"points","name":"81.1","legendgroup":"81.1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia"],"y":[81.2],"text":"wykres$Country: Australia<br />wykres$life: 81.2<br />factor(wykres$life): 81.2","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(252,97,214,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(252,97,214,1)"}},"hoveron":"points","name":"81.2","legendgroup":"81.2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia","Australia"],"y":[81.3,81.3],"text":"wykres$Country: Australia<br />wykres$life: 81.3<br />factor(wykres$life): 81.3","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(253,97,208,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(253,97,208,1)"}},"hoveron":"points","name":"81.3","legendgroup":"81.3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Austria"],"y":[81.4],"text":"wykres$Country: Austria<br />wykres$life: 81.4<br />factor(wykres$life): 81.4","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,97,202,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,97,202,1)"}},"hoveron":"points","name":"81.4","legendgroup":"81.4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Austria"],"y":[81.5],"text":"wykres$Country: Austria<br />wykres$life: 81.5<br />factor(wykres$life): 81.5","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,97,196,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,97,196,1)"}},"hoveron":"points","name":"81.5","legendgroup":"81.5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia"],"y":[81.7],"text":"wykres$Country: Australia<br />wykres$life: 81.7<br />factor(wykres$life): 81.7","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,98,190,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,98,190,1)"}},"hoveron":"points","name":"81.7","legendgroup":"81.7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia"],"y":[81.9],"text":"wykres$Country: Australia<br />wykres$life: 81.9<br />factor(wykres$life): 81.9","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,99,184,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,99,184,1)"}},"hoveron":"points","name":"81.9","legendgroup":"81.9","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia","Austria"],"y":[82,82],"text":["wykres$Country: Australia<br />wykres$life: 82.0<br />factor(wykres$life): 82","wykres$Country: Austria<br />wykres$life: 82.0<br />factor(wykres$life): 82"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,100,177,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,100,177,1)"}},"hoveron":"points","name":"82","legendgroup":"82","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia"],"y":[82.3],"text":"wykres$Country: Australia<br />wykres$life: 82.3<br />factor(wykres$life): 82.3","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,101,171,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,101,171,1)"}},"hoveron":"points","name":"82.3","legendgroup":"82.3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia"],"y":[82.5],"text":"wykres$Country: Australia<br />wykres$life: 82.5<br />factor(wykres$life): 82.5","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,103,164,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,103,164,1)"}},"hoveron":"points","name":"82.5","legendgroup":"82.5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia"],"y":[82.7],"text":"wykres$Country: Australia<br />wykres$life: 82.7<br />factor(wykres$life): 82.7","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,105,157,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,105,157,1)"}},"hoveron":"points","name":"82.7","legendgroup":"82.7","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia"],"y":[82.8],"text":"wykres$Country: Australia<br />wykres$life: 82.8<br />factor(wykres$life): 82.8","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,107,149,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,107,149,1)"}},"hoveron":"points","name":"82.8","legendgroup":"82.8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia"],"y":[83],"text":"wykres$Country: Australia<br />wykres$life: 83.0<br />factor(wykres$life): 83","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(254,109,142,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(254,109,142,1)"}},"hoveron":"points","name":"83","legendgroup":"83","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Austria","Austria"],"y":[84,84],"text":"wykres$Country: Austria<br />wykres$life: 84.0<br />factor(wykres$life): 84","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(253,111,134,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(253,111,134,1)"}},"hoveron":"points","name":"84","legendgroup":"84","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Australia"],"y":[86],"text":"wykres$Country: Australia<br />wykres$life: 86.0<br />factor(wykres$life): 86","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(252,113,126,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(252,113,126,1)"}},"hoveron":"points","name":"86","legendgroup":"86","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["Austria","Austria"],"y":[88,88],"text":"wykres$Country: Austria<br />wykres$life: 88.0<br />factor(wykres$life): 88","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(250,116,118,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(250,116,118,1)"}},"hoveron":"points","name":"88","legendgroup":"88","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"category","autorange":true,"range":[0.4,10.6],"tickmode":"auto","ticktext":["Afghanistan","Albania","Algeria","Angola","Antigua and Barbuda","Argentina","Armenia","Australia","Austria","Azerbaijan"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["Afghanistan","Albania","Algeria","Angola","Antigua and Barbuda","Argentina","Armenia","Australia","Austria","Azerbaijan"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Country","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":true,"range":[43.165,90.135],"tickmode":"auto","ticktext":["50","60","70","80","90"],"tickvals":[50,60,70,80,90],"categoryorder":"array","categoryarray":["50","60","70","80","90"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Year","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.93503937007874},"annotations":[{"text":"factor(wykres$life)","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"385443334592":{"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"385443334592","visdat":{"385443334592":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

\#<a name="9"></a> \# Zapewnienie powtarzalności, Regresor

``` r
df <- data_without_na_in_life_expectancy

rdf <- df%>% select(Life.expectancy, Status, Adult.Mortality, infant.deaths, Alcohol, percentage.expenditure, Hepatitis.B, Measles, BMI, under.five.deaths, Polio, Total.expenditure, Diphtheria, HIV.AIDS, thinness..1.19.years, thinness.5.9.years, Income.composition.of.resources, Schooling)

inTraining <- createDataPartition(y = rdf$Life.expectancy, p = .8, list = FALSE)
training <- rdf[inTraining, ]
testing <- rdf[-inTraining, ]
ctrl <- trainControl(method = "repeatedcv", number = 4,repeats = 10)
fitLm <- train(Life.expectancy ~ .,
                data = training,
                method = "lm",
                metric = "RMSE",
                trControl = ctrl)
lmPredict<-predict(fitLm, newdata=testing)
postResample(lmPredict,testing$Life.expectancy)
```

    ##      RMSE  Rsquared       MAE 
    ## 4.4216112 0.7769966 3.3310023

\#<a name="10"></a> \# Analiza ważnosci atrybutów

``` r
modelValues <- data.frame(obs = testing$Life.expectancy, pred = lmPredict)

importance <- varImp(fitLm, scale = FALSE)
ggplot(importance)
```

![](final_files/figure-markdown_github/analiza-1.png)

Powyższy rozkład cech w regresorze wskazał na znaczące powiązanie
długości życia do wskaźnika śmiertelności u osób chorych na AIDS, oraz
odsetkiem umieralności u osób dorosłych w przedziale 15-60 lat. W
ostatnich latach choroba AIDS przestała być klasyfikowana jako choroba
śmiertelna a zaklasyfikowana została jako choroba przewlekła. Stan ten
dotyczy jednak krajów “rozwiniętych”

Korelacja cech zachorowań na AIDS oraz niskiej długości uczenszczania do
szkół powoduje brak znajomości metod antykoncepcji, co potęguje stopień
zachorowań.

Trzecim co do ważności atrybutem jest długość nauki w szkołach. Niższa
przewidywana długość życia należy do krajów rozwijających się. W tych
krajach dostęp do nauki i szkół jest ograniczony co pośrednio może
wpływać na wazność tej cechy w regresorze.

Najmniej ważnym spośród wykorzystanych cech jest niedowaga w wieku
dziecięco-młodzieńczym oraz poziom spożywanego alkoholu.
