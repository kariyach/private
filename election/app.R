# 必要なパッケージのインストール
if (!requireNamespace("shiny")) install.packages("shiny")
if (!requireNamespace("dplyr")) install.packages("dplyr")
if (!requireNamespace("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("purrr")) install.packages("purrr")
if (!requireNamespace("scales")) install.packages("scales")
if (!requireNamespace("paletteer")) install.packages("paletteer")

library(readr)
library(shiny)
library(dplyr)
library(ggplot2)
library(purrr)
library(scales)
library(paletteer)  # カラーパレット用

library(rsconnect)
# rsconnect::deployApp('election/app.R')

df_pr_parties_2021 <- read_csv("data/pr_parties/hr2021_pr_parties.csv") |> 
  mutate(, .before = block,
         year = 2021
  ) 

df_pr_parties_2024 <- read_csv("data/pr_parties/hr2024_pr_parties.csv") |> 
  mutate(, .before = block,
         year = 2024
  )

df_pr_parties <- 
  bind_rows(df_pr_parties_2021, df_pr_parties_2024) |> 
  mutate(
    block = factor(block, levels = c("北海道", "東北", "北関東", "南関東", "東京", "北陸信越", "東海", "近畿", "中国", "四国", "九州"))
  )

主要政党 <- c("自由民主党", "立憲民主党", "公明党", "金沢桜丘", "国民民主党", "日本維新の会", "日本共産党", "社会民主党", "れいわ新選組")

# データ準備
主要政党 <- c("自由民主党", "立憲民主党", "公明党", "国民民主党", "日本維新の会", "参政党", "日本共産党", "社会民主党", "れいわ新選組")

df_pr_parties <- df_pr_parties %>%
    group_by(year, party, block) %>% 
    summarise(votes = sum(votes), .groups = "drop") %>% 
    filter(party %in% 主要政党) %>%
    mutate(party = factor(party, levels = 主要政党)) 

# UI 定義
ui <- fluidPage(
    titlePanel("衆議院議員選挙：2021 vs. 2024"),
    sidebarLayout(
        sidebarPanel(
            selectInput("selected_block", "地域ブロックを選択:", 
                        choices = unique(df_pr_parties$block))
        ),
        mainPanel(
            plotOutput("votesPlot")
        )
    )
)

# サーバー ロジック
server <- function(input, output) {
    output$votesPlot <- renderPlot({
        # 選択されたブロックに基づいてフィルタリング
        plot_data <- df_pr_parties %>%
            filter(block == input$selected_block)
        
        ggplot(plot_data, aes(x = party, y = votes, fill = as.factor(year))) +
            geom_col(position = "dodge") +
            scale_y_continuous(labels = comma) +
            labs(title = paste(input$selected_block, "の選挙結果"),
                 subtitle = "主要政党の得票数比較",
                 x = "", y = "得票数", fill = "年") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_fill_paletteer_d("awtools::ppalette")
    })
}

# アプリケーションの起動
shinyApp(ui = ui, server = server)