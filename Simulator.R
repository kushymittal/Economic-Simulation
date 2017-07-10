#library(tidyverse)
#library(gganimate)

NUMPLAYERS = 5
ROUNDS = 100
INITWEALTH = 5

#initialize the bank
#columns wealths of the NUMPLAYERS players
#rows show wealths of each of the ROUNDS ticks of the clocks
bank = matrix(0, nrow = ROUNDS, ncol = NUMPLAYERS)
bank[1,] =  c(rep(INITWEALTH, NUMPLAYERS))

# That's the max amount of money that can be lent in an entire simulation
money_lender = NUMPLAYERS*ROUNDS

# money in circulation
init_money = money_lender + NUMPLAYERS*INITWEALTH

# store how much each player has borrowed from the atm
loans = matrix(0, nrow = ROUNDS, ncol = NUMPLAYERS)
loans[1, ] = c(rep(0, NUMPLAYERS))

#function to give a dollar to someone other than oneself
get_recipient = function(player) {
  sample(setdiff(1:NUMPLAYERS, player), 1)
}

#execute trades and update the ledger 
for (i in 2:ROUNDS) 
{
  #every player chooses another person to receive a buck
  recipients = sapply(bank[i - 1,], get_recipient)
  
  #table of the dollars owed each person
  count_table = table(recipients)
  
  # get the indices of the people owed money
  indices = as.integer(names(count_table))

  # borrow money if you're at or below 0
  loans[i, ] = ifelse(bank[i-1,] <= 0, loans[i-1,] + 1, loans[i-1,])
  bank[i, ] = ifelse(bank[i-1,] <= 0, bank[i-1,] + 1, bank[i-1,])
  
  # everyone gives up a dollar
  bank[i, ] = bank[i, ] - 1
  
  # take the change out of the atm
  money_lender = money_lender - sum(loans[i,]-loans[i-1,])
  
  # selected people receive dollars
  bank[i, indices] = bank[i, indices] + count_table
  
  # if your bank balance is high enough, return the loan
  #ifelse( (loans[i,] != 0) && (bank[i,] >= 2*loans[i,]), {bank[i,] = bank[i,] - loans[i,];money_lender = money_lender + loans[i,]; loans[i,]=0}, "Do Nothing Here")
  
  for(x in 1:NUMPLAYERS)
  {
    flip = ((loans[i,x] >= 2) && (bank[i,x] > 2))
    if (flip)
    {
      print("Someone just returned a loan")
      bank[i,x] = bank[i,x] - 2
      money_lender = money_lender + 2
      loans[i,x] = loans[i,x] - 2
    }
  }
  
  
  # Ensure that amount of money in simulation doesn't change
  stopifnot(money_lender + sum(bank[i,]) == init_money)
}

####################Animate it
#Make a suitable long data frame
df = as.data.frame(bank)
names(df) = 1:NUMPLAYERS
df = df %>%
  mutate(frame = 1:ROUNDS) %>%
  gather(person, wealth, 1:NUMPLAYERS) %>%
  mutate(person = as.numeric(person)) %>%
  arrange(frame) %>%
  group_by(frame) %>%
  mutate(rank = rank(wealth, ties.method = "random")) %>%
  ungroup() %>%
  gather(histtype,playerid,c(person,rank)) %>%
  mutate(histtype = sprintf("Ordered by %s", histtype))

p <- ggplot(df, aes(x = playerid, y = wealth, frame = frame, fill=histtype)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_rect(aes( xmin = playerid - .4, xmax = playerid +.4, ymin = 0, ymax = wealth)) +
  scale_x_continuous(breaks = 1:NUMPLAYERS) +
  coord_cartesian(xlim = c(0, NUMPLAYERS), y = c(0, 5 * INITWEALTH)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='players',y='dollars') +
  facet_wrap( ~ histtype,ncol=1) +
  theme(legend.position = "none")
p

#set options for the animation package. Need ImageMagick installed on your computer
animation::ani.options(nmax = ROUNDS,
                       convert = 'C:\\Program Files\\ImageMagick-7.0.6-Q16')
#save the movie
gganimate(p, "dollar_stacked.mp4", interval = .01)
