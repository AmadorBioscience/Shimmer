UIdashboard <- function() {
  tablerDashBody(
    column(12,align = "center",
      tablerProfileCard(
        width = 12,
        title = "Regeneron Pharmaceuticals ",
        subtitle = "Science to Medicine",
        background = "regn.jpg",
        src = "https://media.glassdoor.com/sqll/981/regeneron-pharmaceuticals-squarelogo-1452804357483.png",
        
        tablerSocialLinks(
          tablerSocialLink(
            name = "confluence",
            href = "https://www.confluence.com",
            icon = "confluence"
          ),
          tablerSocialLink(
            name = "twitter",
            href = "https://www.twitter.com/Regeneron",
            icon = "twitter"
          ),
          tablerSocialLink(
            name = "facebook",
            href = "https://www.facebook.com/Regeneron",
            icon = "facebook"
      ))))
    )
}