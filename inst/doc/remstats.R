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

# Set weights to 1 (no event weighting)
history$weight <- 1

# Define effects
effects <- ~ 1 + send("extraversion", info) + inertia()

# Prepare event history
reh <- remify::remify(edgelist = history, model = "tie")

# Compute statistics
stats <- remstats(reh = reh, tie_effects = effects)

# Estimate model parameters (requires remstimate)
# fit <- remstimate::remstimate(reh = reh, stats = stats, method = "MLE")

## -----------------------------------------------------------------------------
head(history)

## -----------------------------------------------------------------------------
head(info)

## -----------------------------------------------------------------------------
history$weight <- 1
reh <- remify::remify(edgelist = history, model = "tie")

## -----------------------------------------------------------------------------
effects <- ~ inertia(scaling = "std")
out <- remstats(tie_effects = effects, reh = reh)

## -----------------------------------------------------------------------------
dim(out)

## -----------------------------------------------------------------------------
out

## -----------------------------------------------------------------------------
head(attr(out, "riskset"))

## -----------------------------------------------------------------------------
effects <- ~ inertia(scaling = "std") + 
             send("extraversion", info) + 
             inertia(scaling = "std"):send("extraversion", info)
out <- remstats(tie_effects = effects, reh = reh)
out

## -----------------------------------------------------------------------------
# Full memory (default)
out_full <- remstats(
    tie_effects  = ~ inertia(),
    reh          = reh,
    memory       = "full"
)

# Window memory: only events within the last 500 time units
out_window <- remstats(
    tie_effects  = ~ inertia(),
    reh          = reh,
    memory       = "window",
    memory_value = 500
)

# Decay memory: exponential decay with half-life 200
out_decay <- remstats(
    tie_effects  = ~ inertia(),
    reh          = reh,
    memory       = "decay",
    memory_value = 200
)

## -----------------------------------------------------------------------------
reh_typed <- remify::remify(
    edgelist   = history,
    model      = "tie",
    event_type = "setting"
)

## -----------------------------------------------------------------------------
out_ignore <- remstats(
    tie_effects = ~ inertia(consider_type = "ignore"),
    reh         = reh_typed
)
dim(out_ignore)       # one inertia slice plus baseline
dimnames(out_ignore)[[3]]

## -----------------------------------------------------------------------------
out_separate <- remstats(
    tie_effects = ~ inertia(consider_type = "separate"),
    reh         = reh_typed
)
dim(out_separate)       # two inertia slices (one per type) plus baseline
dimnames(out_separate)[[3]]

## -----------------------------------------------------------------------------
out_interact <- remstats(
    tie_effects = ~ inertia(consider_type = "interact"),
    reh         = reh_typed
)
dim(out_interact)
dimnames(out_interact)[[3]]

## -----------------------------------------------------------------------------
reh <- remify::remify(edgelist = history, model = "tie")

out_sampled <- remstats(
    tie_effects = ~ inertia() + send("extraversion", info),
    reh         = reh,
    sampling    = TRUE,
    samp_num    = 20,
    seed        = 42
)

## -----------------------------------------------------------------------------
reh <- remify::remify(edgelist = history, model = "actor")

## -----------------------------------------------------------------------------
out <- remstats(
    sender_effects = ~ outdegreeSender(),
    reh            = reh
)

## -----------------------------------------------------------------------------
names(out)

## -----------------------------------------------------------------------------
out

## -----------------------------------------------------------------------------
out <- remstats(
    sender_effects   = ~ outdegreeSender(),
    receiver_effects = ~ inertia(),
    reh              = reh
)

## -----------------------------------------------------------------------------
# Label columns with receiver names and rows with senders
dimnames(out$receiver_stats)[[2]] <- reh$meta$dictionary$actors$actorName
dimnames(out$receiver_stats)[[1]] <- reh$edgelist$actor1[-1]
head(out$receiver_stats[,, "inertia"])

