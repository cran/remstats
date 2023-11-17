## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(remstats)

## -----------------------------------------------------------------------------
# Load data
data(history)
data(info)

# Define effects
effects <- ~ 1 + send("extraversion", info) + inertia()

# Prepare event history with the 'remify package'
rehObject <- remify::remify(edgelist = history, model = "tie")

# Compute statistics
statsObject <- remstats(reh = rehObject, tie_effects = effects)

# Estimate model parameters with the 'remstimate' package
# fit <- remstimate::remstimate(reh = rehObject, stats = statsObject,
# 	method = "MLE", timing = "interval")

## -----------------------------------------------------------------------------
head(history)

## -----------------------------------------------------------------------------
history$weight <- 1
reh <- remify::remify(edgelist = history, model = "tie")

## -----------------------------------------------------------------------------
head(info)

## -----------------------------------------------------------------------------
effects <- ~ inertia(scaling = "std")

## -----------------------------------------------------------------------------
out <- remstats(tie_effects = effects, reh = reh)

## -----------------------------------------------------------------------------
dim(out)

## -----------------------------------------------------------------------------
out

## -----------------------------------------------------------------------------
head(attr(out, "riskset"))

## -----------------------------------------------------------------------------
effects <- ~ inertia(scaling = "std") + send("extraversion", info) + 
    inertia(scaling = "std"):send("extraversion", info) 
out <- remstats(tie_effects = effects, reh = reh)

## -----------------------------------------------------------------------------
reh <- remify::remify(edgelist = history, model = "actor")

## -----------------------------------------------------------------------------
effects <- ~ outdegreeSender()
out <- remstats(sender_effects = effects, reh = reh)

## -----------------------------------------------------------------------------
names(out)

## -----------------------------------------------------------------------------
out

## -----------------------------------------------------------------------------
sender_effects <- ~ outdegreeSender()
receiver_effects <- ~ inertia()
out <- remstats(sender_effects = sender_effects, receiver_effects = receiver_effects, reh = reh)

## -----------------------------------------------------------------------------
# Set the column names equal to the receivers
colnames(out$receiver_stats) <- attributes(reh)$dictionary$actors$actorName
# Set the rownames equal to the senders
rownames(out$receiver_stats) <- reh$edgelist$actor1
# View the first six lines
head(out$receiver_stats[,,"inertia"])

