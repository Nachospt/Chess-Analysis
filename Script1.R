############################################# Chess Analysis #############################################

# start stuff 
#pgn = readLines("Kings_gambit.pgn", warn = FALSE)
#pgn <- paste(pgn, collapse = "\n")


g.fen.plotter <- function(x.lim = c(0,16), y.lim = c(0,16), test.string ="r2q1rk1/pp2p1bp/1np3p1/2Q2p2/3P1B1P/3BPN2/bP3PP1/3RK2R b K h3 0 15"   )
{
  polygon(c(x.lim, rev(x.lim)), c(y.lim[1], y.lim[1], y.lim[2], y.lim[2]), col= grey(0.8), border = F  )
  yb = seq(y.lim[1], y.lim[2], length.out = 9)
  xb = seq(x.lim[1], x.lim[2], length.out = 9)
  
  k= 0
  for ( i in 1:8)
  {
    for (j in 1:8)
    {
      
      if ( (i+j+1)%%2 == 1)
      {
        polygon(c(xb[i], xb[i+1], xb[i+1], xb[i]), c(yb[j], yb[j], yb[j+1], yb[j+1]), col="darkolivegreen3", border =F  )
      }
      
    }
    
  }
  
  k= 0
  i = 1
  rank = 8
  file = 1
  while (k == 0)
  {
    temp = substr(test.string, start = i, stop= i)
    
    if (temp %in% c("r", "R", "n", "N", "b", "B", "q", "Q", "k", "K", "p", "P") )
    {
      x.c = 0.5*(xb[file] + xb[file+1])
      y.c = 0.5*(yb[rank] + yb[rank+1])
      
      c.col = "white"
      l.col = "black"
      if (temp %in% c("r","n", "b",  "q",  "k",  "p" ) )
      {
        c.col = "black"
        l.col = "white"
        
      }
      
      symbols(x = x.c, y = y.c, fg = c.col, circles = 0.5*0.8*(yb[2]-yb[1]) , add= T, inches = F, bg =c.col )
      text(temp, x = x.c, y = y.c, col = l.col , cex = 0.3 )
      i = i + 1
      file = file +1
    }
    
    if (temp == "/" )
    {
      file = 1
      rank = rank -1
      i = i +1
    }    
    
    if (temp %in% c(1,2,3,4,5,6,7,8))
    {
      file = file + as.numeric(temp)
      i = i + 1
    }
    
    if (temp == " ")
    {
      k = 1    
    }
    print(temp)
    print(rank)
    print(file)
    print("-------")
    
  } 
  
}


pgn = readLines("one_nf3.pgn", warn = FALSE)
pgn <- paste(pgn, collapse = "\n")
#pgn = paste( "[Event", unlist(strsplit(pgn, split = "[Event", fixed = TRUE ) )[1:100], sep="" )

n          = length(unlist(strsplit(pgn, split = "[Event", fixed = TRUE)))
junk       = Chess$new()
fen.long   = junk$fen()
fen.part   = unlist(strsplit(fen.long, split = " ", fixed = TRUE))
fen.short  = paste(fen.part[1],fen.part[2],fen.part[3],fen.part[4],fen.part[5], 13, sep=" ")

fen.list   = fen.short
fen.p.list = fen.short
con.x      = 1
con.y      = 1
max.move   = 0
U.list     = fen.list
out        = matrix(c(fen.list, rep("NP", 30), "NP", rep("NP2", 30)), nrow=31, ncol=2) 
out2       = out
out2[1,1]  = "start"
ngame.mat  = matrix(rep(0, 62), nrow=31, ncol=2)
score.mat  = matrix(rep(NA, 62), nrow=31, ncol=2) 

# get the fen strings 
order = sample(1:n, n)
ii = 0

for ( i in order  )
{
  ii = ii +1
  temp <- Chess$new()
  
  pgn.temp = paste( "[Event", unlist(strsplit(pgn, split = "[Event", fixed = TRUE ) )[i], sep="" )
  temp$load_pgn(pgn.temp)
  m        = temp$history()
  #print(m)
  print(length(m))
  
  if ( length(m >= 3))
  {
    J        = min(c(29, length(m)))
    
    
    
    loss   = grepl("0-1", pgn.temp)
    win    = grepl("1-0", pgn.temp)
    draw   = grepl("1/2-1/2", pgn.temp)
    
    score  = 1*win +0.5*draw + 0
    print(score)
    ngames = 1
    
    
    pos.p = Chess$new()
    pos   = Chess$new()
    pos.n = Chess$new()
    for ( j in 1:J)
    {
      #print(pos$fen())
      #print(m[j])
      #print(paste(ncol(out), "is n col", nrow(out), "is n row", con.x, "con.x", con.y, "con.y",  sep = " "))
      
      
      fen.long = pos$fen()
      fen.part = unlist(strsplit(fen.long, split = " ", fixed = TRUE))
      fen.short = paste(fen.part[1],fen.part[2],fen.part[3],fen.part[4],fen.part[5], 13, sep=" ")
      fen.old = fen.short
      pos$move(m[j]) 
      
      fen.long = pos$fen()
      fen.part = unlist(strsplit(fen.long, split = " ", fixed = TRUE))
      fen.short = paste(fen.part[1],fen.part[2],fen.part[3],fen.part[4],fen.part[5], 13, sep=" ")
      game = i -1
      move = j -1
      
      #print(paste(con.x, con.y))
      
      #print(fen.short %in% U.list)
      
      if ( ((fen.short %in% U.list ) == F)  )
      {
        
        if (con.y <= 29) {con.y = con.y + 1}
        
        #print(paste(ncol(out), "is n col", nrow(out), "is n row", con.x, "con.x", con.y, "con.y",  sep = " "))
        if(out[con.y, con.x] != "NP") 
        {
          
          #print(paste(ncol(out), "is n col", nrow(out), "is n row", con.x, "con.x", con.y, "con.y",  sep = " "))
          while ( max(which(out[1:con.y,con.x + 1] == "NP"  ))  >= con.y ) {con.x = con.x +1}
          
          
          #print(paste(ncol(out), "is n col", nrow(out), "is n row", con.x, "con.x", con.y, "con.y",  sep = " "))
          
          
          out = cbind(out[,1:con.x], matrix("NP", nrow=31, ncol=1), out[,(con.x+1):ncol(out)])
          out2 = cbind(out2[,1:con.x], matrix("NP", nrow=31, ncol=1), out2[,(con.x+1):ncol(out2)])
          ngame.mat =cbind(ngame.mat[,1:con.x], matrix(0, nrow=31, ncol=1), ngame.mat[,(con.x+1):ncol(ngame.mat)])
          score.mat =cbind(score.mat[,1:con.x], matrix(NA, nrow=31, ncol=1), score.mat[,(con.x+1):ncol(score.mat)])
          con.x = con.x + 1
          
          #print(paste(ncol(out), "is n col", nrow(out), "is n row", con.x, "con.x", con.y, "con.y",  sep = " "))
          
        }
        
        #print(paste(ncol(out), "is n col", con.x, "con.x", con.y, "con.y",  sep = " "))
        out[con.y, con.x] = fen.short
        out2[con.y, con.x] = m[j]
        ngame.mat[con.y, con.x] = 1
        score.mat[con.y, con.x] = score
      }
      
      if ( (fen.short %in% U.list )  )
      {    
        temp = which(out==fen.short, arr.ind = TRUE)
        con.x = temp[2]
        con.y = temp[1]
        
        #print(paste(ncol(out), "is n col", con.x, "con.x", con.y, "con.y",  sep = " "))
        ngame.mat[con.y, con.x] = ngame.mat[con.y, con.x]  + 1
        score.mat[con.y, con.x] = score.mat[con.y, con.x] + score
        
      }
      
      
      
      
      #print(paste(m[j],out2[con.y, con.x] , con.y, con.x , score))
      #$print("----------")
      
      fen.list   = c(fen.list, fen.short)
      fen.p.list = c(fen.p.list, fen.old)
      U.list     = unique(fen.list)
      
      
    }
    #print(out2)
  }
  print(paste("game", i, "index", ii, sep = " "))
  
}

out.con.f = data.frame(fen.list, fen.p.list, CON = 1)


out.con  = ddply(out.con.f,~fen.list+fen.p.list,summarise, CON = sum(CON ) )


# plot hte move list 
ymax = 112.5 +5
ydelta = 4
Ni = nrow(out)
Nj = ncol(out) -1 
out = out[,1:Nj]
MNG = 20


max.ng.list = 1:Nj 

for ( i in 1:Nj)
{
  max.ng.list[i] = max(ngame.mat[,i]) 
}

out       = out[,max.ng.list>=MNG
                score.mat = score.mat[,max.ng.list>=MNG
                                      ngame.mat = ngame.mat[,max.ng.list>=MNG
                                                            Pscore    = score.mat/ngame.mat
                                                            plot.mat  = 0*ngame.mat
                                                            for ( i in 1:ncol(plot.mat))
                                                            {
                                                              for (j in 1:nrow(plot.mat) )
                                                              {
                                                                if (max(ngame.mat[j:31,i]) >= 2) {plot.mat[j,i] = 1} 
                                                              } 
                                                            }
                                                            
                                                            
                                                            
                                                            Nj = ncol(out)
                                                            
                                                            
                                                            #pdf(file ="test.pdf", height= 3.5*72, width = 3.5*72)
                                                            pdf(file ="test.pdf", height= 36, width = 48)
                                                            
                                                            par(pty="m", mar = c(0,0,0,0))
                                                            
                                                            plot(1,1, xlim = c(0.5, 2*Nj + 1), ylim = c(0, ymax ), yaxs = "i", xaxs = "i", ylab = "Move", xaxt="n" ,type="n", asp=1) 
                                                            axis(3, at= mean(c(0.5, 2*Nj + 1)), label = "1 Nf3 ... ", mgp = c(-2, -7, 0), tick = F, cex.axis = 10 )
                                                            
                                                            #out.con = out.con[out.con$CON >= 5]
                                                            for (i in 1:nrow(out.con))
                                                            {
                                                              temp.1 = which(out==out.con$fen.list[i], arr.ind = TRUE)
                                                              temp.2 = which(out==out.con$fen.p.list[i], arr.ind = TRUE)
                                                              
                                                              print(temp.1)
                                                              if ( (length(temp.1) >= 1) & (length(temp.2) >= 1)  )
                                                              {
                                                                print(temp.1)
                                                                print(ngame.mat[temp.1[1], temp.1[2]] )
                                                                
                                                                if ( (plot.mat[temp.1[1],temp.1[2]] == 1) & (plot.mat[temp.2[1],temp.2[2]] == 1)  ) # (max(ngame.mat[temp.1[1],(temp.1[2]:31)], na.rm =T) >= 2) & max((ngame.mat[temp.2[1], temp.2[2]:31], na.rm =T) >= 2) 
                                                                {
                                                                  x.c.1 = 2* temp.1[2] 
                                                                  y.c.1 = ymax - 1.25*3*(temp.1[1] + 1) - 1  +ydelta 
                                                                  x.c.2 = 2* temp.2[2]
                                                                  y.c.2 = ymax - 1.25*3*(temp.2[1] + 1)  -1 +ydelta 
                                                                  
                                                                  noise = 0.3*runif(1) - 0.15
                                                                  noise.x = 0.3*runif(1) - 0.15
                                                                  g.col = sample(c("blue", "red", "orange", "green", "black" ), 1)
                                                                  noise.l = runif(1)
                                                                  
                                                                  
                                                                  segments(x0 = x.c.1+noise.x,  y0 = y.c.1     ,                 x1 = x.c.1+ noise.x, y1 = 0.5*y.c.1+0.5*y.c.2 +noise , lwd=1.5 + noise.l,                  col=g.col  )
                                                                  segments(x0 = x.c.1+noise.x,  y0 = 0.5*y.c.1+0.5*y.c.2 +noise ,x1 = x.c.2+ noise.x, y1 = 0.5*y.c.1+0.5*y.c.2 +noise , lwd=1.5 + noise.l, col=g.col  )
                                                                  segments(x0 = x.c.2+noise.x,  y0 = y.c.2     ,                 x1 = x.c.2+ noise.x, y1 = 0.5*y.c.1+0.5*y.c.2 +noise , lwd=1.5 + noise.l,                  col=g.col  )
                                                                  
                                                                  
                                                                }
                                                              }
                                                            }
                                                            
                                                            
                                                            
                                                            #par(fig = c(0.25, 0.75, 0.25, 0.75), new = TRUE, mar = c(0,0,0,0)  )
                                                            #ggchessboard()
                                                            
                                                            
                                                            
                                                            
                                                            for ( i in 1:Ni)
                                                            {
                                                              for (j in 1:Nj)
                                                              {
                                                                
                                                                print(ngame.mat[i,j:31])
                                                                
                                                                if ( (out[i,j] != "NP")  & (plot.mat[i,j] == 1 ) )# &  (ngame.mat[i,j] >= MNG ) & (max(ngame.mat[i,j:31], na.rm =T) >= MNG)
                                                                {
                                                                  x.c = 2* j 
                                                                  y.c = ymax - 1.25*3*(i + 1) -1 +ydelta 
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  #par(fig = c(0.25, 0.75, 0.25, 0.75), new = TRUE, mar = c(0,0,0,0)  )
                                                                  #plot(out[i,j], type = "ggplot")
                                                                  
                                                                  g.fen.plotter(x.lim = c(x.c -0.85,x.c +0.85), y.lim = c(y.c - 0.85,y.c + 0.85), test.string =out[i,j] )
                                                                  
                                                                  
                                                                  #polygon( x= c(x.c -0.5, x.c  -0.5, x.c +0.5, x.c  + 0.5  ), y= c(y.c - 0.5, y.c + 0.5, y.c + 0.5, y.c - 0.5), col="grey"     )  
                                                                  
                                                                  
                                                                  
                                                                  text(x = x.c -0.4  , y = y.c +1.25*1 , labels = ngame.mat[i,j], cex = 0.65) 
                                                                  
                                                                  text(x = x.c + 0.4 , y = y.c +1.25*1 , labels = 100*round(Pscore[i,j],2), cex = 0.65) 
                                                                  
                                                                }
                                                              }
                                                              
                                                            }
                                                            
                                                            
                                                            #vb = seq(0, ymax, length.out =37)
                                                            
                                                            #for (i in 1:length(vb))
                                                            #{abline(h = vb[i])}
                                                            
                                                            
                                                            dev.off()