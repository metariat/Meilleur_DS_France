emblem.plot = function(df.in, var.x, var.y){
  #This function superimpose the mean of the response variable over an x variable
  # @df.in: the data that contains var.x and var.y
  # @var.x: variable on the x-axis
  # @var.y: variable on the y-axis
  x_ = data.frame(table(df.in[, get(var.x)]))
  y_ = df.in[, list("response" = mean(get(var.y))), get(var.x)]
  x_ = merge(x_, y_, by.x = "Var1", by.y = "get")
  p_ = plot_ly() %>%
    add_trace(x = ~x_$Var1, y = x_$Freq, type = 'bar', name = var.x,
              marker = list(color = '#C9EFF9'),
              hoverinfo = "text") %>%
    add_trace(x = ~x_$Var1, y = ~x_$response, type = 'scatter', mode = 'lines', name = 'response', yaxis = 'y2',
              line = list(color = '#45171D'),
              hoverinfo = "text") %>%
    layout(title = paste0("histogram of ", i),
           xaxis = list(title = i),
           yaxis = list(side = 'left', title = '', showgrid = FALSE, zeroline = FALSE),
           yaxis2 = list(side = 'right', overlaying = "y", title = 'response', showgrid = FALSE, zeroline = FALSE))
  return(p_)
}
