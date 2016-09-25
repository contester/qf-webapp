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

notifyMe(title, icon, body) ->
    if Notification.permission == "granted"
      showNotification(title, icon, body)
    else
      Notification.requestPermission().then(-> showNotification(title, icon, body))