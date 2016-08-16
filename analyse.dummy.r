## first adhoc analysis of game results
#
# just a warm up for upcomming projects

# read everything into matrix like objects (indexing with the name of the team is an additional feature)
team.names <- sort(union(unique(game.results$Team1),unique(game.results$Team2)))
Set1.Points.Mat <- data.frame(row.names = team.names)
Set2.Points.Mat <- data.frame(row.names = team.names)
Set3.Points.Mat <- data.frame(row.names = team.names)
Total.Set.Points.Mat <- data.frame(row.names = team.names)
Set.Score.Mat <- data.frame(row.names = team.names)
  
for (gi in 1:nrow(game.results)) {
  Set1.Points.Mat[game.results$Team1[gi],game.results$Team2[gi]] <- game.results$Set1.Points[gi]
  Set2.Points.Mat[game.results$Team1[gi],game.results$Team2[gi]] <- game.results$Set2.Points[gi]
  Set3.Points.Mat[game.results$Team1[gi],game.results$Team2[gi]] <- game.results$Set3.Points[gi]
  Total.Set.Points.Mat[game.results$Team1[gi],game.results$Team2[gi]] <- game.results$Total.Set.Points[gi]
  Set.Score.Mat[game.results$Team1[gi],game.results$Team2[gi]] <- game.results$Set.Score[gi]
  # skew symmetric!!
  Set1.Points.Mat[game.results$Team2[gi],game.results$Team1[gi]] <- -game.results$Set1.Points[gi]
  Set2.Points.Mat[game.results$Team2[gi],game.results$Team1[gi]] <- -game.results$Set2.Points[gi]
  Set3.Points.Mat[game.results$Team2[gi],game.results$Team1[gi]] <- -game.results$Set3.Points[gi]
  Total.Set.Points.Mat[game.results$Team2[gi],game.results$Team1[gi]] <- -game.results$Total.Set.Points[gi]
  Set.Score.Mat[game.results$Team2[gi],game.results$Team1[gi]] <- -game.results$Set.Score[gi]
}
Set1.Points.Mat <- Set1.Points.Mat[,order(colnames(Set1.Points.Mat))]
Set2.Points.Mat <- Set2.Points.Mat[,order(colnames(Set2.Points.Mat))]
Set3.Points.Mat <- Set3.Points.Mat[,order(colnames(Set3.Points.Mat))]
Total.Set.Points.Mat <- Total.Set.Points.Mat[,order(colnames(Total.Set.Points.Mat))]
Set.Score.Mat <- Set.Score.Mat[,order(colnames(Set.Score.Mat))]

# Note: NA on diagonal, since a team can't play against itself


## starting analysis
Games.won <- rowSums(x = sign(Set.Score.Mat), na.rm = TRUE)
acc.Set.Score <- rowSums(x = Set.Score.Mat, na.rm = TRUE)
acc.Set.Points <- rowSums(x = Total.Set.Points.Mat, na.rm = TRUE)
acc.Set1.Points <- rowSums(x = Set1.Points.Mat, na.rm = TRUE)
acc.Set2.Points <- rowSums(x = Set2.Points.Mat, na.rm = TRUE)
acc.Set3.Points <- rowSums(x = Set3.Points.Mat, na.rm = TRUE)
acc.Set12.Points <- acc.Set1.Points + acc.Set2.Points
# Note that Points from Set 1 and 2 are more expressive than points from all three sets, since not all games last for 3 sets

rank.Games.won <- order(Games.won, decreasing = TRUE)
rank.Set.Score <- order(acc.Set.Score, decreasing = TRUE)
rank.Set.Points <- order(acc.Set.Points, decreasing = TRUE)
rank.Set1.Points <- order(acc.Set1.Points, decreasing = TRUE)
rank.Set2.Points <- order(acc.Set2.Points, decreasing = TRUE)
rank.Set3.Points <- order(acc.Set3.Points, decreasing = TRUE)
rank.Set12.Points <- order(acc.Set12.Points, decreasing = TRUE)

# looking for differences in rankings (compute Kendall's tau - permutation distance)
rank.tau <- cor(cbind(acc.Games.won, acc.Set.Score, acc.Set.Points, acc.Set1.Points, acc.Set2.Points, acc.Set3.Points, acc.Set12.Points), method = "kendall")

# Monte Carlo test of rank stability
generateRandGameOrders <- function(n.teams) {
  # generate game data
  pv <- choose(c(5:9, 9:5), c(0:4, 4:0))
  pv <- pv / sum(pv)
  Set1.Points.Mat <- matrix(data = sample(c(-5:-1,1:5), size = n.teams^2, prob = pv, replace = TRUE), ncol = n.teams, nrow = n.teams)
  diag(Set1.Points.Mat) <- 0
  Set2.Points.Mat <- matrix(data = sample(c(-5:-1,1:5), size = n.teams^2, prob = pv, replace = TRUE), ncol = n.teams, nrow = n.teams)
  diag(Set2.Points.Mat) <- 0
  Set3.Points.Mat <- matrix(data = sample(c(-5:-1,1:5), size = n.teams^2, prob = pv, replace = TRUE), ncol = n.teams, nrow = n.teams)
  diag(Set3.Points.Mat) <- 0
  Set3.Points.Mat[sign(Set1.Points.Mat) == sign(Set2.Points.Mat)] <- 0
  # aggregate game data
  Set12.Points.Mat <- Set1.Points.Mat + Set2.Points.Mat
  Set123.Points.Mat <- Set12.Points.Mat + Set3.Points.Mat
  Set.Score.Mat <- sign(Set1.Points.Mat) + sign(Set2.Points.Mat) + sign(Set3.Points.Mat)
  # generate matrix
  acc.Mat  <- cbind(acc.Games.won = rowSums(x = sign(Set.Score.Mat)),
                    acc.Set.Score = rowSums(x = Set.Score.Mat),
                    acc.Set.Points = rowSums(x = Set123.Points.Mat),
                    acc.Set1.Points = rowSums(x = Set1.Points.Mat),
                    acc.Set2.Points = rowSums(x = Set2.Points.Mat),
                    acc.Set3.Points = rowSums(x = Set3.Points.Mat),
                    acc.Set12.Points = rowSums(x = Set12.Points.Mat)
  )
  return(cor(acc.Mat, method = "kendall"))
}

n.tests <- 1000 # number of tests to run
# mc.values <- matrix(data = NA, nrow = n.tests, ncol = length(rank.tau))
# for (ri in 1:n.tests) {
#   mc.values[ri,] <- as.vector(generateRandGameOrders(n.teams = length(team.names)))
# }
mc.values <- t(sapply(1:n.tests, function(ri) { as.vector(generateRandGameOrders(n.teams = length(team.names))) }))
matrix(colMeans(mc.values[!is.na(rowSums(mc.values)),]), nrow = 7, ncol = 7) # filtering neccessary, since sets without any variance can be created
sum(is.na(rowSums(mc.values))) # how many are not rankable by random drawing?


# advanced monte carlo test of rank stability: now with more realistic data (team power changes between sets are less likely)
generateRandGameOrders2 <- function(n.teams) {
  # generate game data
  pv <- choose(c(5:9, 9:5), c(0:4, 4:0))
  pv <- pv / sum(pv)
  Set1.Points.Mat <- matrix(data = sample(c(-5:-1,1:5), size = n.teams^2, prob = pv, replace = TRUE), ncol = n.teams, nrow = n.teams)
  diag(Set1.Points.Mat) <- 0
  pv2n <- c(choose(c(5:9, 8:5), c(0:4, 3:0)),0)
  pv2n <- pv2n / sum(pv2n)
  pv2p <- c(0, choose(c(5:8, 9:5), c(1:4, 4:0)))
  pv2p <- pv2p / sum(pv2p)
  Set2.Points.Mat <- matrix(data = 0, ncol = n.teams, nrow = n.teams)
  Set2.Points.Mat[sign(Set1.Points.Mat) ==  1] <- sample(c(-5:-1,1:5), size = n.teams^2, prob = pv2p, replace = TRUE)
  Set2.Points.Mat[sign(Set1.Points.Mat) == -1] <- sample(c(-5:-1,1:5), size = n.teams^2, prob = pv2n, replace = TRUE)
  Set3.Points.Mat <- matrix(data = sample(c(-5:-1,1:5), size = n.teams^2, prob = pv, replace = TRUE), ncol = n.teams, nrow = n.teams)
  diag(Set3.Points.Mat) <- 0
  Set3.Points.Mat[sign(Set1.Points.Mat) == sign(Set2.Points.Mat)] <- 0
  # aggregate game data
  Set12.Points.Mat <- Set1.Points.Mat + Set2.Points.Mat
  Set123.Points.Mat <- Set12.Points.Mat + Set3.Points.Mat
  Set.Score.Mat <- sign(Set1.Points.Mat) + sign(Set2.Points.Mat) + sign(Set3.Points.Mat)
  # generate matrix
  acc.Mat  <- cbind(acc.Games.won = rowSums(x = sign(Set.Score.Mat)),
                    acc.Set.Score = rowSums(x = Set.Score.Mat),
                    acc.Set.Points = rowSums(x = Set123.Points.Mat),
                    acc.Set1.Points = rowSums(x = Set1.Points.Mat),
                    acc.Set2.Points = rowSums(x = Set2.Points.Mat),
                    acc.Set3.Points = rowSums(x = Set3.Points.Mat),
                    acc.Set12.Points = rowSums(x = Set12.Points.Mat)
  )
  return(cor(acc.Mat, method = "kendall"))
}

n.tests <- 1000 # number of tests to run
# mc.values <- matrix(data = NA, nrow = n.tests, ncol = length(rank.tau))
# for (ri in 1:n.tests) {
#   mc.values[ri,] <- as.vector(generateRandGameOrders(n.teams = length(team.names)))
# }
mc.values2 <- t(sapply(1:n.tests, function(ri) { as.vector(generateRandGameOrders2(n.teams = length(team.names))) }))
matrix(colMeans(mc.values2[!is.na(rowSums(mc.values2)),]), nrow = 7, ncol = 7) # filtering neccessary, since sets without any variance can be created
sum(is.na(rowSums(mc.values2))) # how many are not rankable by random drawing?

t.test(mc.values, mc.values2)