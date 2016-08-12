## create dummy data
#
# structure: like 2. Greifswalder Strandturnier
# 5 teams,  10 games (everybody against everybody), set games of 3 (5 points each)
#
# Note: The number of sets is hard-wired into the structure of the data.
#       This has to be changed in future applications - sufficient for now.

# intialize structure
game.results <- data.frame(Game = 1:10,
                          Team1 = rep(NA,10),
                          Team2 = rep(NA,10),
                          Set1.Points = rep(NA,10),
                          Set2.Points = rep(NA,10),
                          Set3.Points = rep(NA,10),
                          Set.Score = rep(NA,10),
                          Total.Set.Points = rep(NA,10),
                          Notes = rep("",10))

# Fill data
c.game <- 1
for (team1 in 1:4) {
  for (team2 in (team1+1):5) {
    # enter team names
    game.results$Team1[c.game] <- paste("Team", toString(team1))
    game.results$Team2[c.game] <- paste("Team", toString(team2))
    # scores are counted from the point of View of the first team
    # draw points at random, no ties, draws follow binom distribution
    game.results$Set1.Points[c.game] <- sample(c(-5:-1,1:5), 1, p = choose(c(5:9, 9:5), c(0:4, 4:0)))
    game.results$Set2.Points[c.game] <- sample(c(-5:-1,1:5), 1, p = choose(c(5:9, 9:5), c(0:4, 4:0)))
    if (sign(game.results$Set1.Points[c.game]) != sign(game.results$Set2.Points[c.game])) {
      game.results$Set3.Points[c.game] <- sample(c(-5:-1,1:5), 1, p = choose(c(5:9, 9:5), c(0:4, 4:0)))
    } # generate thrid game only if the first two have different results
    game.results$Set.Score[c.game] <- sum(sign(c(game.results$Set1.Points[c.game], game.results$Set2.Points[c.game], game.results$Set3.Points[c.game])), na.rm = TRUE)
    game.results$Total.Set.Points[c.game] <- sum(c(game.results$Set1.Points[c.game], game.results$Set2.Points[c.game], game.results$Set3.Points[c.game]), na.rm = TRUE)
    
    c.game <- c.game + 1
  }
}

View(game.results)
