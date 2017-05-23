package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"runtime/debug"

	"github.com/getsentry/raven-go"
	"github.com/gin-gonic/gin"
)

// Adapted from https://github.com/gin-gonic/gin/issues/274

var (
	errAuth         = newPartyError(http.StatusSeeOther, "Authentication Error", "Could not login via Spotify.")
  errUnauthorized = newPartyError(http.StatusUnauthorized, "Unauthorized", "Unauthorized request made to Party")
	errBadRequest   = newPartyError(http.StatusBadRequest, "Bad Request", "Bad request made to Party")
	errInternal     = newPartyError(http.StatusInternalServerError, "Internal Error", "An unexpected error occurred with Party")
)

type partyErrors struct {
	Errors []*partyError `json:"errors"`
}

type partyError struct {
	Code    int    `json:"status"`
	Title   string `json:"title"`
	Message string `json:"detail"`
	Meta    struct {
		Cause  error `json:"cause"`
		trace  *[]byte
		Detail *string `json:"details"`
	} `json:"meta"`
}

func (e *partyError) Error() string {
	if e.HasCause() {
		if e.Meta.trace == nil {
			return fmt.Sprintf("%d: %s\n%s", e.Code, *e.Meta.Detail, e.Meta.Cause)
		}

		trace := *e.Meta.trace
		n := bytes.IndexByte(trace, 0)
		if n > 0 {
			return fmt.Sprintf("%d: %s\n%s\n\n%s\n", e.Code, *e.Meta.Detail, e.Meta.Cause, string(trace[:n]))
		}

		return fmt.Sprintf("%d: %s\n%s\n", e.Code, *e.Meta.Detail, e.Meta.Cause)
	}

	if e.HasDetail() {
		return fmt.Sprintf("%d: %s\n%s", e.Code, e.Message, *e.Meta.Detail)
	}

	return fmt.Sprintf("%d: %s", e.Code, e.Message)
}

func (e *partyError) WithDetail(detail string) *partyError {
	// Send the error off to Sentry
	raven.CaptureError(e, map[string]string{
		"detail": detail,
	})

	// Add detail to provided partyError
	augmentedError := *e
	augmentedError.Meta.Detail = &detail
	return &augmentedError
}

func (e *partyError) HasDetail() bool {
	return e.Meta.Detail != nil
}

func (e *partyError) CausedBy(err error) *partyError {
	augmentedError := *e
	if gin.IsDebugging() {
		trace := debug.Stack()
		augmentedError.Meta.trace = &trace
	}
	augmentedError.Meta.Cause = err
	return &augmentedError
}

func (e *partyError) HasCause() bool {
	return e.Meta.Cause != nil
}

func newPartyError(code int, title string, publicMessage string) *partyError {
	return &partyError{
		Code:    code,
		Title:   title,
		Message: publicMessage,
	}
}

func handleErrors() gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Next() // Run through all handlers

		numErrors := len(c.Errors)
		// Bail if there are no errors
		if numErrors == 0 {
			return
		}

		errors := make([]*partyError, numErrors)
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
					session := DefaultSession(c)
					errorJSON, _ := json.Marshal(err)
					session.Flash("error", string(errorJSON))

					c.Redirect(err.Code, "/")
					return
				case http.StatusUnauthorized:
					c.Header("Www-Authenticate", fmt.Sprintf(`Bearer realm="spotify", error="unauthorized", error_description="%s"`, err.Message))
				}

				// Otherwise add to list of errors
				errors[i] = err
				code = err.Code
			default:
				errors[i] = errInternal
				code = errInternal.Code
			}
		}

		// Respond with errors
		c.JSON(code, partyErrors{
			Errors: errors,
		})
	}
}
