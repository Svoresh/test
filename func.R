sq9 <- function(x){
  if (x == 1) return(315) else
    if (x < 1) return(NA) else {
      cycleNum <- trunc(sqrt((x - 1) / 4) - 0.5) + 1
      return((135 + cycleNum * 180 + (x - 1) * 45  / cycleNum) %% 360)
    }
}

findud <- function(v){
  vud <- v[-1] - v[-length(v)]
  return(ifelse(vud > 0, 1, ifelse(vud < 0, -1, 0)))
}

aiobar <- function(h = c(), l = c()){
  hl <- rep(NA, length(h))
  hl[1] <- l[1] #для первого бара всегда фиксируем основание
  #HH
  hh <- which(diff(h)>0)+1
  ll <- which(diff(l)>=0)+1
  hl[intersect(hh, ll)] <- h[intersect(hh, ll)]
  #LL
  hh <- which(diff(h)<=0)+1
  ll <- which(diff(l)<0)+1
  hl[intersect(hh, ll)] <- l[intersect(hh, ll)]
  #OHL
  hh <- which(diff(h)>0)+1
  ll <- which(diff(l)<0)+1
  ohl <- intersect(hh, ll)
  if (length(ohl) != 0){
    hih <- rep(max(ohl), length(ohl))
    lol <- rep(max(ohl), length(ohl))
    ihl <- rep(max(ohl), length(ohl))
    
    for (j in seq(ohl)) {
      #search HH
      for (i in 2:(ohl[j]-1)){
        if (h[ohl[j]] > h[(ohl[j]-i)] && l[ohl[j]] >= l[(ohl[j]-i)]) {
          hih[j] <- i
          break
        }
      }### <- search HH
      #search LL
      for (i in 2:(ohl[j]-1)){
        if (l[ohl[j]] < l[(ohl[j]-i)] && h[ohl[j]] <= h[(ohl[j]-i)]) {
          lol[j] <- i
          break
        }
      } ### <- search LL
      #search IHL
      for (i in 2:(ohl[j]-1)){
        if (l[ohl[j]] >= l[(ohl[j]-i)] && h[ohl[j]] <= h[(ohl[j]-i)]) {
          ihl[j] <- i
          break
        }
      } ### <- search IHL
      hl[ohl[j]] <- if (hih[j]<lol[j] && hih[j]<ihl[j]) h[ohl[j]] 
                     else {
                       if (hih[j]>lol[j] && lol[j]<ihl[j]) l[ohl[j]] else
                       ifelse(hl[(ohl[j]-i)] == h[(ohl[j]-i)], l[ohl[j]], h[ohl[j]])
                     }
    }
  }
  
  #IHL
  hh <- which(diff(h)<=0)+1
  ll <- which(diff(l)>=0)+1
  ihl <- intersect(hh, ll)
  if(length(ihl) != 0){
    for (j in seq(ihl)) {
      for (i in 0:(ihl[j]-1)) {
        if (hl[(ihl[j]-1)] == h[(ihl[j]-1)]) hl[ihl[j]] <- l[ihl[j]] else
          # if (hl[(ihl[j]-1)] == l[(ihl[j]-1)]) 
          hl[ihl[j]] <- h[ihl[j]]
      }
    }
  }
  hl
}

peaks <- function(x, values = FALSE){
  res <- sort(unique(c(which(diff(sign(diff(c(Inf, x, Inf)))) == -2),
                       which(diff(sign(diff(c(Inf, -x, Inf)))) == 2))))
  if (values) return(abs(x[res])) else return(res)
}

swings <- function(x){ # загружаем результат aiobar
  ph <- peaks(x) # номера баров вершин
  pl <- peaks(-x) # номера баров оснований
  fudH <- c(ph[1], ph[which(findud(peaks(x, T)) == 1) + 1])
  fudL <- c(pl[1], pl[which(findud(peaks(-x, T)) == -1) + 1])
  #hl <- sort(c(fudH, fudL))
  
  hl <- c()
  jl <- split(fudH, findInterval(fudH, fudL))
  for (i in seq(jl)) {
    j <- unlist(jl[i], use.names=FALSE)
    hl <- c(hl, j[which.max(x[j])])
  }
  jh <- split(fudL, findInterval(fudL, fudH))
  for (i in seq(jh)) {
    j <- unlist(jh[i], use.names=FALSE)
    hl <- c(hl, j[which.min(x[j])])
  }
  hl <- sort(hl)
  # return(list(h=fudH, l=fudL))
  return(hl)
}
