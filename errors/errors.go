package errors

import (
	"bytes"
	"fmt"
	"net/http"
	"runtime/debug"

	"github.com/getsentry/raven-go"
	"github.com/gin-gonic/gin"
)

// Adapted from https://github.com/gin-gonic/gin/issues/274

var (
	// Auth provides a new Authentication error builder
	Auth = newPartyError(http.StatusSeeOther, "Authentication Error", "Could not login via Spotify.")
	// Forbidden provides a new Forbidden error builder
	Forbidden = newPartyError(http.StatusForbidden, "Forbidden", "Forbidden request made to Party")
	// Unauthorized provides a new Unauthorized error builder
	Unauthorized = newPartyError(http.StatusUnauthorized, "Unauthorized", "Unauthorized request made to Party")
	// BadRequest provides a new Bad Request error builder
	BadRequest = newPartyError(http.StatusBadRequest, "Bad Request", "Bad request made to Party")
	// NotFound provides a new Not Found error builder
	NotFound = newPartyError(http.StatusNotFound, "Not Found", "Requested resource was not found")
	// Internal provides a new Internal Server Error error builder
	Internal = newPartyError(http.StatusInternalServerError, "Internal Error", "An unexpected error occurred with Party")
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

		if e.HasDetail() {
			return fmt.Sprintf("%d: %s\n%s\n", e.Code, *e.Meta.Detail, e.Meta.Cause)
		}

		return fmt.Sprintf("%d: %s\n", e.Code, e.Meta.Cause)
	}

	if e.HasDetail() {
		return fmt.Sprintf("%d: %s\n%s", e.Code, e.Message, *e.Meta.Detail)
	}

	return fmt.Sprintf("%d: %s", e.Code, e.Message)
}

func (e *partyError) WithMessage(message string) *partyError {
	// Replace message in given partyError
	augmentedError := *e
	augmentedError.Message = message
	return &augmentedError
}

func (e *partyError) WithDetail(detail string) *partyError {
	// Send the error off to Sentry
	raven.CaptureError(e, map[string]string{
		"detail": detail,
	})

	// Add detail to given partyError
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
