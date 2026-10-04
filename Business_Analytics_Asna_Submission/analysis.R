options(width = 95, digits = 7)

dir.create("results", recursive = TRUE, showWarnings = FALSE)

d <- read.csv("Telecom_Info_Updated_V2 (1).csv")

d$customer.type <- factor(d$customer.type, levels = c("Individual", "Business", "VIP"))

v <- names(d)[1:6]

xs <- v[1:5]

y <- d$ChurnProbability

out <- function(name, expr) {
    z <- capture.output(eval.parent(substitute(expr)))
    writeLines(z, paste0("results/", name, ".txt"))
    cat(paste(z, collapse = "\n"), "\n")
}

out("audit", {
    print(dim(d))
    print(colSums(is.na(d)))
    cat("Duplicate full rows:", sum(duplicated(d)), "\n")
    cat("Target outside [0,1]:", sum(y < 0 | y > 1), "\n")
    cat("Fractional complaints:", sum(abs(d$NumComplaints - round(d$NumComplaints)) > 1e-08), "\n")
    print(tools::md5sum("Telecom_Info_Updated_V2 (1).csv"))
    print(sessionInfo())
})

desc <- do.call(rbind, lapply(v, function(n) {
    x <- d[[n]]
    tt <- table(x)
    modes <- names(tt)[tt == max(tt)]
    data.frame(variable = n, n = length(x), mean = mean(x), median = median(x), sd = sd(x), min = min(x), 
        max = max(x), IQR = IQR(x), mode = if (max(tt) == 1) 
            "No unique mode"
        else paste(modes, collapse = "; "), mode_frequency = max(tt))
}))

write.csv(desc, "results/descriptive.csv", row.names = FALSE)

out("descriptive", print(desc[, c(1, 3:7, 9, 10)], row.names = FALSE))

groups <- do.call(rbind, lapply(levels(d$customer.type), function(g) {
    x <- y[d$customer.type == g]
    se <- sd(x)/sqrt(length(x))
    data.frame(group = g, n = length(x), mean = mean(x), sd = sd(x), low = mean(x) - qt(0.975, length(x) - 
        1) * se, high = mean(x) + qt(0.975, length(x) - 1) * se, min = min(x), max = max(x))
}))

write.csv(groups, "results/groups.csv", row.names = FALSE)

av <- aov(ChurnProbability ~ customer.type, d)

dev <- abs(y - ave(y, d$customer.type, FUN = median))

bf <- aov(dev ~ d$customer.type)

wa <- oneway.test(ChurnProbability ~ customer.type, d, var.equal = FALSE)

pw <- pairwise.t.test(y, d$customer.type, p.adjust.method = "holm", pool.sd = FALSE)

out("anova", {
    print(groups, row.names = FALSE)
    cat("\nBrown-Forsythe variance test\n")
    print(summary(bf))
    print(wa)
    cat("\nHolm-adjusted pairwise Welch tests\n")
    print(pw)
    cat("\nOrdinary ANOVA sensitivity check\n")
    print(summary(av))
    cat("Eta squared:", summary(av)[[1]][1, 2]/sum(summary(av)[[1]][, 2]), "\n")
    print(kruskal.test(ChurnProbability ~ customer.type, d))
    cat("Threshold-rule agreement:", mean(as.character(d$customer.type) == ifelse(y < 3.5, "Individual", 
        ifelse(y < 5.5, "Business", "VIP"))), "\n")
})

normal <- do.call(rbind, lapply(v, function(n) {
    t <- shapiro.test(d[[n]])
    data.frame(variable = n, W = unname(t$statistic), p = t$p.value)
}))

write.csv(normal, "results/normality.csv", row.names = FALSE)

out("normality", print(normal, row.names = FALSE))

corr <- do.call(rbind, lapply(xs, function(n) {
    t <- cor.test(d[[n]], y, method = "spearman", exact = FALSE)
    data.frame(variable = n, rho = unname(t$estimate), S = unname(t$statistic), p = t$p.value, pearson = cor(d[[n]], 
        y))
}))

corr$p_holm <- p.adjust(corr$p, "holm")

write.csv(corr, "results/correlation.csv", row.names = FALSE)

out("correlation", print(corr, row.names = FALSE))

m <- lm(ChurnProbability ~ MonthlyCharges + DataUsageGB + CallMinutes + CustomerTenureMonths + NumComplaints, 
    d)

co <- cbind(coef(summary(m)), confint(m))

write.csv(co, "results/coefficients.csv")

vif <- sapply(xs, function(n) 1/(1 - summary(lm(reformulate(setdiff(xs, n), n), d))$r.squared))

X <- model.matrix(m)

inv <- solve(crossprod(X))

adj <- resid(m)^2/(1 - hatvalues(m))^2

hc <- inv %*% crossprod(X, X * adj) %*% inv

hcse <- sqrt(diag(hc))

hct <- coef(m)/hcse

robust <- data.frame(beta = coef(m), HC3_SE = hcse, t = hct, p = 2 * pt(abs(hct), df.residual(m), lower.tail = FALSE))

write.csv(robust, "results/hc3.csv")

set.seed(2026)

train <- sample(seq_len(nrow(d)), 4000)

mt <- lm(formula(m), d[train, ])

pred <- predict(mt, d[-train, ])

actual <- y[-train]

rmse <- sqrt(mean((actual - pred)^2))

mae <- mean(abs(actual - pred))

r2 <- 1 - sum((actual - pred)^2)/sum((actual - mean(actual))^2)

simple <- do.call(rbind, lapply(xs, function(n) {
    f <- lm(reformulate(n, "ChurnProbability"), d)
    z <- coef(summary(f))
    data.frame(variable = n, intercept = coef(f)[1], slope = coef(f)[2], p = z[2, 4], R2 = summary(f)$r.squared)
}))

write.csv(simple, "results/simple.csv", row.names = FALSE)

out("regression", print(summary(m)))

out("diagnostics", {
    print(vif)
    print(robust)
    print(shapiro.test(resid(m)))
    cat("Maximum Cook distance:", max(cooks.distance(m)), "\n")
    cat("Holdout n=", length(actual), " RMSE=", rmse, " MAE=", mae, " R2=", r2, "\n")
    cat("Baseline holdout RMSE=", sqrt(mean((actual - mean(y[train]))^2)), "\n")
    cat("Holdout predicted range:", range(pred), "\n")
    print(simple, row.names = FALSE)
    print(cor(d[xs]))
})

save(d, m, av, wa, normal, corr, desc, groups, vif, robust, mt, train, file = "results/analysis.RData")

cols <- c("#167D9A", "#D0782D", "#6750A4", "#27856D", "#BA4A67")

labs <- c("Monthly charges (USD)", "Data usage (GB)", "Call minutes", "Customer tenure (months)", "Complaint measure", 
    "Churn score (0 to 10)")

pngstart <- function(file, w = 1800, h = 1050) {
    png(paste0("results/", file, ".png"), width = w, height = h, res = 180)
    par(mar = c(4.4, 4.7, 3.5, 1.4), family = "sans", col.axis = "#374151", col.lab = "#1F2937", fg = "#718096", 
        cex = 1.1)
}

for (j in c(1:4, 6)) {
    x <- d[[v[j]]]
    color <- cols[if (j == 6) 
        5
    else j]
    pngstart(paste0("bell", j))
    h <- hist(x, breaks = 30, plot = FALSE)
    xx <- seq(mean(x) - 3.5 * sd(x), mean(x) + 3.5 * sd(x), length.out = 600)
    plot(h, freq = FALSE, col = adjustcolor(color, 0.25), border = "white", main = paste("Distribution of", 
        tolower(labs[j])), xlab = labs[j], ylab = "Density", xlim = range(xx), ylim = c(0, max(h$density, 
        dnorm(xx, mean(x), sd(x))) * 1.15))
    grid(nx = NA, ny = NULL, col = "#E5E7EB")
    lines(xx, dnorm(xx, mean(x), sd(x)), col = color, lwd = 3)
    abline(v = mean(x), col = "#D0782D", lwd = 2, lty = 2)
    abline(v = median(x), col = "#27856D", lwd = 2, lty = 3)
    legend("topright", legend = c("Observed histogram", "Normal reference", sprintf("Mean %.2f", mean(x)), 
        sprintf("Median %.2f", median(x))), col = c(adjustcolor(color, 0.45), color, "#D0782D", "#27856D"), 
        lwd = c(8, 3, 2, 2), lty = c(1, 1, 2, 3), bty = "n", cex = 0.85)
    dev.off()
}

pngstart("group", 1800, 1050)

boxplot(ChurnProbability ~ customer.type, d, col = c("#A9DCCC", "#A7D3E2", "#D5C4EA"), border = c("#27856D", 
    "#167D9A", "#6750A4"), ylab = "Churn score (0 to 10)", xlab = "Customer type", main = "Churn scores by customer type", 
    outline = TRUE)

points(1:3, groups$mean, pch = 18, cex = 1.5, col = "#B85B27")

abline(h = c(3.5, 5.5), lty = 3, col = "#9CA3AF")

legend("topleft", "Diamond = group mean", pch = 18, col = "#B85B27", bty = "n")

dev.off()

pngstart("qq", 1800, 1400)

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1), cex = 0.9)

for (j in 1:6) {
    qqnorm(d[[v[j]]], main = labs[j], pch = 16, cex = 0.35, col = "#167D9A")
    qqline(d[[v[j]]], col = "#D0782D", lwd = 2)
}

dev.off()

pngstart("scatter", 1800, 1450)

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1), cex = 0.9)

for (j in 1:5) {
    plot(d[[xs[j]]], y, pch = 16, cex = 0.35, col = adjustcolor(cols[j], 0.19), xlab = labs[j], ylab = "Churn score", 
        main = sprintf("Spearman rho = %.3f", corr$rho[j]))
    abline(lm(y ~ d[[xs[j]]]), col = cols[j], lwd = 3)
}

plot.new()

text(0.5, 0.65, "All 5,000 observations", cex = 1.15)

text(0.5, 0.5, "Lines show simple linear fits")

text(0.5, 0.35, "Association is not causation")

dev.off()

pngstart("diagnostic", 1800, 1250)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), cex = 0.9)

plot(m, which = c(1, 2, 3, 5), col = adjustcolor("#167D9A", 0.35), pch = 16, cex = 0.35)

dev.off()

cols <- c("#087F8C", "#D17A22", "#7554A3", "#29806C", "#BB4567")

labs <- c("Monthly charges (USD)", "Data usage (GB)", "Call minutes", "Customer tenure (months)", "Complaint measure", 
    "Churn score (0 to 10)")

for (k in seq_along(c(1:4, 6))) {
    j <- c(1:4, 6)[k]
    x <- d[[j]]
    mu <- mean(x)
    s <- sd(x)
    xx <- seq(mu - 3.5 * s, mu + 3.5 * s, length.out = 1000)
    yy <- dnorm(xx, mu, s)
    png(paste0("results/curve", j, ".png"), 1800, 1050, res = 180)
    par(mar = c(4.5, 4.5, 3.7, 1.3), family = "sans", col.axis = "#3A3A3A", cex = 1.05)
    plot(xx, yy, type = "n", main = paste("Normal reference curve for", tolower(labs[j])), xlab = labs[j], 
        ylab = "Probability density", ylim = c(0, max(yy) * 1.16), bty = "l")
    grid(nx = NA, ny = NULL, col = "#E9E9E9")
    polygon(c(xx, rev(xx)), c(yy, rep(0, length(yy))), col = adjustcolor(cols[k], alpha.f = 0.12), border = NA)
    lines(xx, yy, col = cols[k], lwd = 3.5)
    segments(mu, 0, mu, dnorm(mu, mu, s), col = cols[k], lwd = 2, lty = 2)
    legend("topright", legend = c(sprintf("Mean = %.2f", mu), sprintf("SD = %.2f", s)), text.col = "#333333", 
        bty = "n", cex = 0.9)
    dev.off()
}

z <- cor(d[1:6], method = "pearson")

labs <- c("Charges", "Data GB", "Calls", "Tenure", "Complaints", "Churn")

png("results/heatmap.png", 1800, 1600, res = 180)

par(mar = c(6, 7, 4, 3), family = "sans", xpd = FALSE)

pal <- colorRampPalette(c("#7651A8", "#FFFFFF", "#087F8C"))(201)

plot(c(0.5, 6.5), c(0.5, 6.5), type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1, main = "Pearson correlation matrix")

for (i in 1:6) for (j in 1:6) {
    col <- pal[round((z[i, j] + 1) * 100) + 1]
    rect(j - 0.5, 6 - i + 0.5, j + 0.5, 6 - i + 1.5, col = col, border = "white")
    text(j, 7 - i, sprintf("%.2f", z[i, j]), col = if (abs(z[i, j]) > 0.65) 
        "white"
    else "#202020", cex = 1.2)
}

axis(1, at = 1:6, labels = labs, las = 2, tick = FALSE, cex.axis = 1.05)

axis(2, at = 1:6, labels = rev(labs), las = 1, tick = FALSE, cex.axis = 1.05)

mtext("Purple: negative     White: zero     Teal: positive     Scale: -1 to +1", side = 3, line = 0.4, 
    cex = 0.85)

dev.off()

write.csv(z, "results/pearson_matrix.csv")

out("group_normality", print(by(y, d$customer.type, shapiro.test)))

pearson_tests <- lapply(xs, function(n) cor.test(d[[n]], y, method = "pearson"))

pearson_results <- data.frame(variable = xs, r = sapply(pearson_tests, function(test) unname(test$estimate)), 
    t = sapply(pearson_tests, function(test) unname(test$statistic)), df = sapply(pearson_tests, function(test) unname(test$parameter)), 
    p = sapply(pearson_tests, function(test) test$p.value))

pearson_results$p_holm <- p.adjust(pearson_results$p, "holm")

write.csv(pearson_results, "results/pearson_tests.csv", row.names = FALSE)

out("pearson_tests", print(pearson_results, row.names = FALSE))

out("simple_models", {
    for (n in xs) {
        cat(n, "\n")
        print(summary(lm(reformulate(n, "ChurnProbability"), d)))
    }
})

