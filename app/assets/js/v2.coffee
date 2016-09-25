cleanupText(body) ->
    div = document.createElement("div")
    div.innerHTML = body
    div.innerText || div.textContent || div.text

showNotification(title, icon, body) ->
    notification = new Notification(title, {
      icon: icon,
      body: text,
    })

    notification.addEventListener('click', ->
        window.focus()
        @close()
    )

    window.setTimeout(->
        notification.close();
    , 600000);

