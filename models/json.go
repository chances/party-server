package models

// JSON API Specification - Document Structure
// http://jsonapi.org/format/1.0/#document-structure

type Response struct {
	Data ResponseData `json:"data"`
	Meta
}

type ResponseData struct {
	ID            string      `json:"id"`
	Type          string      `json:"type"`
	Attributes    interface{} `json:"attributes"`
	Relationships interface{} `json:"relationships,omitempty"`
	Links         Links       `json:"links,omitempty"`
	Meta
}

type Relationship struct {
	Links Links        `json:"links,omitempty"`
	Data  ResponseData `json:"data,omitempty"`
	Meta
}

type Links struct {
	Self    string `json:"self,omitempty"`
	Related string `json:"related,omitempty"`
}

type Link struct {
	Href string `json:"href"`
	Meta
}

type Meta struct {
	Meta interface{} `json:"meta,omitempty"`
}

func NewResponse(id, resourceType, selfUri string, attributes interface{}) Response {
	return Response{
		Data: ResponseData{
			ID:         id,
			Type:       resourceType,
			Attributes: attributes,
			Links: Links{
				Self: selfUri,
			},
		},
	}
}

var EmptyRespose = Response{}
