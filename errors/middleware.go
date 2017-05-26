package errors

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"

	"github.com/chances/chances-party/session"
	"github.com/gin-gonic/gin"
)

func HandleErrors() gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Next() // Run through all handlers

		numErrors := len(c.Errors)
		// Bail if there are no errors
		if numErrors == 0 {
			return
		}

		allErrors := make([]*partyError, numErrors)
		var code int

		// Add all errors to an array, following the Errors JSON API spec
		// http://jsonapi.org/format/#errors
		for i, ginError := range c.Errors {
			fmt.Fprintf(os.Stderr, "%s\n", ginError)
			// IDEA: Send the error detail and cause to some log manager?

			switch ginError.Err.(type) {
			case *partyError:
				err := ginError.Err.(*partyError)

				// On errAuth flash error to session and redirect to index on errAuth
				switch err.Code {
				case http.StatusSeeOther:
					sesh := session.DefaultSession(c)
					errorJSON, _ := json.Marshal(err)
					sesh.Flash("error", string(errorJSON))

					c.Redirect(err.Code, "/")
					return
				case http.StatusUnauthorized:
					c.Header("Www-Authenticate", fmt.Sprintf(`Bearer realm="spotify", error="unauthorized", error_description="%s"`, err.Message))
				}

				// Otherwise add to list of errors
				allErrors[i] = err
				code = err.Code
			default:
				allErrors[i] = Internal
				code = Internal.Code
			}
		}

		// Respond with errors
		c.JSON(code, partyErrors{
			Errors: allErrors,
		})
	}
}
