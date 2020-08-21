document.querySelector("#@|button-class|")
        .addEventListener("click", (e) => {
          e.preventDefault();
          const postBody = {
            sku: "@|sku|",
            qty: 1
          };
          console.log(postBody);
          fetch("/add", {
            method: "POST",
            body: JSON.stringify(postBody)
          });
        });
