####color palettes for discrete or continuous values####
Palettes <- list(
  
  #---------------------------------------------------------------
  # Primarily Discrete Palettes
  #---------------------------------------------------------------
  

  #19-colors
  mycols_19 <- c("#66CDAA", "#458B74", "#7FFFD4", "#8EE5EE", "#7AC5CD", "#104E8B", "#009ACD", "#EEA2AD", 
                 "#CD8C95", "#EEDD82", "#CDBE70", "#FF7F00", "#CD6600", "#CD2626", "#8B7D6B", "#9F79EE", "#ADADAD", "#FFD700", "#9BCD9B"),
  
  #15-colors
  ironMan = c('#371377','#7700FF','#9E0142','#FF0080', '#DC494C',"#F88D51","#FAD510","#FFFF5F",'#88CFA4',
             '#238B45',"#02401B","#0AD7D3","#046C9A", "#A2A475", 'grey35'),
  
  circus = c("#D52126","#88CCEE", "#FEE52C","#117733", "#CC61B0","#99C945","#2F8AC4", "#332288",
             "#E68316", "#661101","#F97B72", "#DDCC77", "#11A579","#89288F", "#E73F74"),
  mycols_15 <- c("#00CDCD", "#A4D3EE", "#FFA500", "#FFB6C1", "#607B8B", "#8F8F8F", "#4F94CD",
                 "#228B22", "#CD5C5C", "#EEDFCC", "#36648B", "#CD950C", "#009ACD", "#AB82FF", "#8B6969"),
  
  #12-colors
  paired = c("9"="#A6CDE2","1"="#1E78B4","3"="#74C476","12"="#34A047","11"="#F59899","2"="#E11E26",
             "10"="#FCBF6E","4"="#F47E1F","5"="#CAB2D6","8"="#6A3E98","6"="#FAF39B","7"="#B15928"),
  mycols_12 = c("#008B8B", "#7AC5CD", "#00688B", "#9BCD9B", "#9ACD32", "#EECFA1", "#FF6A6A", "#F08080", "#FF8C00", "#8A8A8A", "#EE9A49", "#BC8F8F"),
  
  #11-colors
  grove = c("#1a1334","#01545a","#017351","#03c383","#aad962","#fbbf45","#ef6a32","#ed0345","#a12a5e","#710162","#3B9AB2"),
  mycols_11 = c("cadetblue3", "cyan4", "lightsteelblue3", "darkorange1", "goldenrod1", "tan1", "steelblue4", "olivedrab3", "gray55", "lightcoral", "tan2"),
  #9-colors
  mycols_9 = c("cadetblue3", "cyan4", "lightsteelblue3", "darkorange1", "goldenrod1", "tan1", "steelblue4", "lightcoral", "gray55"),
  #8-colors
  mycols_8 = c("cadetblue3", "cyan4", "lightsteelblue3", "darkorange1", "goldenrod1", "tan1", "steelblue4", "lightcoral"),
  mycols_8_2 = c("#66CDAA", "#B0E2FF", "#00688B", "#B4EEB4", "#FFA07A", "#EEA2AD", "#EECFA1", "#EE9A49"),
  #5-colors
  zissou = c("1"="#3B9AB2", "4"="#78B7C5", "3"="#EBCC2A", "5"="#E1AF00", "2"="#F21A00"), #wesanderson
  darjeeling = c("1"="#FF0000", "2"="#00A08A", "3"="#F2AD00", "4"="#F98400", "5"="#5BBCD6"), #wesanderson
  rushmore = c("1"="#E1BD6D", "5"="#EABE94", "2"="#0B775E", "4"="#35274A" , "3"="#F2300F"), #wesanderson
  captain = c("1"="grey","2"="#A1CDE1","3"="#12477C","4"="#EC9274","5"="#67001E"),
  
  #---------------------------------------------------------------
  # Primarily Continuous Palettes
  #---------------------------------------------------------------
  
  #10-colors
  horizon = c("1"='#000075',"4"='#2E00FF', "6"='#9408F7', "10"='#C729D6', "8"='#FA4AB5', "3"='#FF6A95', "7"='#FF8B74', "5"='#FFAC53', "9"='#FFCD32', "2"='#FFFF60'),
  
  #9-colors
  horizonExtra =c("1"="#000436","4"="#021EA9","6"="#1632FB","8"="#6E34FC","3"="#C732D5","9"="#FD619D","7"="#FF9965","5"="#FFD32B","2"="#FFFC5A"),
  blueYellow = c("1"="#352A86","2"="#343DAE","3"="#0262E0","4"="#1389D2","5"="#2DB7A3","6"="#A5BE6A","7"="#F8BA43","8"="#F6DA23","9"="#F8FA0D"),
  sambaNight = c("6"='#1873CC',"2"='#1798E5',"8"='#00BFFF',"5"='#4AC596',"1"='#00CC00',"4"='#A2E700',"9"='#FFFF00',"7"='#FFD200',"3"='#FFA500'), #buencolors
  solarExtra = c("5"='#3361A5', "7"='#248AF3', "1"='#14B3FF', "8"='#88CEEF', "9"='#C1D5DC', "4"='#EAD397', "3"='#FDB31A',"2"= '#E42A2A', "6"='#A31D1D'),  #buencolors
  whitePurple = c("9"='#f7fcfd',"6"='#e0ecf4',"8"='#bfd3e6',"5"='#9ebcda',"2"='#8c96c6',"4"='#8c6bb1',"7"='#88419d',"3"='#810f7c',"1"='#4d004b'),
  whiteBlue = c("9"='#fff7fb',"6"='#ece7f2',"8"='#d0d1e6',"5"='#a6bddb',"2"='#74a9cf',"4"='#3690c0',"7"='#0570b0',"3"='#045a8d',"1"='#023858'),
  whiteRed = c("1"="white", "2"="red"),
  comet = c("1"="#E6E7E8","2"="#3A97FF","3"="#8816A7","4"="black"),
  
  #7-colors
  greenBlue = c("4"='#e0f3db',"7"='#ccebc5',"2"='#a8ddb5',"5"='#4eb3d3',"3"='#2b8cbe',"6"='#0868ac',"1"='#084081'),
  
  #6-colors
  beach = c("4"="#87D2DB","1"="#5BB1CB","6"="#4F66AF","3"="#F15F30","5"="#F7962E","2"="#FCEE2B"),
  
  #5-colors
  coolwarm = c("1"="#4858A7", "4"="#788FC8", "5"="#D6DAE1", "3"="#F49B7C", "2"="#B51F29"),
  fireworks = c("5"="white","2"="#2488F0","4"="#7F3F98","3"="#E22929","1"="#FCB31A"),
  greyMagma = c("2"="grey", "4"="#FB8861FF", "5"="#B63679FF", "3"="#51127CFF", "1"="#000004FF"),
  fireworks2 = c("5"="black", "2"="#2488F0","4"="#7F3F98","3"="#E22929","1"="#FCB31A"),
  purpleOrange = c("5"="#581845", "2"="#900C3F", "4"="#C70039", "3"="#FF5744", "1"="#FFC30F")
)
