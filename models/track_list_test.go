// This file is generated by SQLBoiler (https://github.com/vattle/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// DO NOT EDIT

package models

import (
	"bytes"
	"reflect"
	"testing"

	"github.com/vattle/sqlboiler/boil"
	"github.com/vattle/sqlboiler/randomize"
	"github.com/vattle/sqlboiler/strmangle"
)

func testTrackLists(t *testing.T) {
	t.Parallel()

	query := TrackLists(nil)

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}
func testTrackListsDelete(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = trackList.Delete(tx); err != nil {
		t.Error(err)
	}

	count, err := TrackLists(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testTrackListsQueryDeleteAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = TrackLists(tx).DeleteAll(); err != nil {
		t.Error(err)
	}

	count, err := TrackLists(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testTrackListsSliceDeleteAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := TrackListSlice{trackList}

	if err = slice.DeleteAll(tx); err != nil {
		t.Error(err)
	}

	count, err := TrackLists(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}
func testTrackListsExists(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	e, err := TrackListExists(tx, trackList.ID)
	if err != nil {
		t.Errorf("Unable to check if TrackList exists: %s", err)
	}
	if !e {
		t.Errorf("Expected TrackListExistsG to return true, but got false.")
	}
}
func testTrackListsFind(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	trackListFound, err := FindTrackList(tx, trackList.ID)
	if err != nil {
		t.Error(err)
	}

	if trackListFound == nil {
		t.Error("want a record, got nil")
	}
}
func testTrackListsBind(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = TrackLists(tx).Bind(trackList); err != nil {
		t.Error(err)
	}
}

func testTrackListsOne(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	if x, err := TrackLists(tx).One(); err != nil {
		t.Error(err)
	} else if x == nil {
		t.Error("expected to get a non nil record")
	}
}

func testTrackListsAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackListOne := &TrackList{}
	trackListTwo := &TrackList{}
	if err = randomize.Struct(seed, trackListOne, trackListDBTypes, false, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}
	if err = randomize.Struct(seed, trackListTwo, trackListDBTypes, false, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackListOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = trackListTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := TrackLists(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 2 {
		t.Error("want 2 records, got:", len(slice))
	}
}

func testTrackListsCount(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	trackListOne := &TrackList{}
	trackListTwo := &TrackList{}
	if err = randomize.Struct(seed, trackListOne, trackListDBTypes, false, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}
	if err = randomize.Struct(seed, trackListTwo, trackListDBTypes, false, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackListOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = trackListTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := TrackLists(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 2 {
		t.Error("want 2 records, got:", count)
	}
}

func testTrackListsInsert(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := TrackLists(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testTrackListsInsertWhitelist(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx, trackListColumnsWithoutDefault...); err != nil {
		t.Error(err)
	}

	count, err := TrackLists(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testTrackListToManyQueueParties(t *testing.T) {
	var err error
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a TrackList
	var b, c Party

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, &a, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}

	randomize.Struct(seed, &b, partyDBTypes, false, partyColumnsWithDefault...)
	randomize.Struct(seed, &c, partyDBTypes, false, partyColumnsWithDefault...)

	b.QueueID = a.ID
	c.QueueID = a.ID
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = c.Insert(tx); err != nil {
		t.Fatal(err)
	}

	party, err := a.QueueParties(tx).All()
	if err != nil {
		t.Fatal(err)
	}

	bFound, cFound := false, false
	for _, v := range party {
		if v.QueueID == b.QueueID {
			bFound = true
		}
		if v.QueueID == c.QueueID {
			cFound = true
		}
	}

	if !bFound {
		t.Error("expected to find b")
	}
	if !cFound {
		t.Error("expected to find c")
	}

	slice := TrackListSlice{&a}
	if err = a.L.LoadQueueParties(tx, false, (*[]*TrackList)(&slice)); err != nil {
		t.Fatal(err)
	}
	if got := len(a.R.QueueParties); got != 2 {
		t.Error("number of eager loaded records wrong, got:", got)
	}

	a.R.QueueParties = nil
	if err = a.L.LoadQueueParties(tx, true, &a); err != nil {
		t.Fatal(err)
	}
	if got := len(a.R.QueueParties); got != 2 {
		t.Error("number of eager loaded records wrong, got:", got)
	}

	if t.Failed() {
		t.Logf("%#v", party)
	}
}

func testTrackListToManyHistoryParties(t *testing.T) {
	var err error
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a TrackList
	var b, c Party

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, &a, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}

	randomize.Struct(seed, &b, partyDBTypes, false, partyColumnsWithDefault...)
	randomize.Struct(seed, &c, partyDBTypes, false, partyColumnsWithDefault...)

	b.HistoryID = a.ID
	c.HistoryID = a.ID
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = c.Insert(tx); err != nil {
		t.Fatal(err)
	}

	party, err := a.HistoryParties(tx).All()
	if err != nil {
		t.Fatal(err)
	}

	bFound, cFound := false, false
	for _, v := range party {
		if v.HistoryID == b.HistoryID {
			bFound = true
		}
		if v.HistoryID == c.HistoryID {
			cFound = true
		}
	}

	if !bFound {
		t.Error("expected to find b")
	}
	if !cFound {
		t.Error("expected to find c")
	}

	slice := TrackListSlice{&a}
	if err = a.L.LoadHistoryParties(tx, false, (*[]*TrackList)(&slice)); err != nil {
		t.Fatal(err)
	}
	if got := len(a.R.HistoryParties); got != 2 {
		t.Error("number of eager loaded records wrong, got:", got)
	}

	a.R.HistoryParties = nil
	if err = a.L.LoadHistoryParties(tx, true, &a); err != nil {
		t.Fatal(err)
	}
	if got := len(a.R.HistoryParties); got != 2 {
		t.Error("number of eager loaded records wrong, got:", got)
	}

	if t.Failed() {
		t.Logf("%#v", party)
	}
}

func testTrackListToManyAddOpQueueParties(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a TrackList
	var b, c, d, e Party

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, &a, trackListDBTypes, false, strmangle.SetComplement(trackListPrimaryKeyColumns, trackListColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}
	foreigners := []*Party{&b, &c, &d, &e}
	for _, x := range foreigners {
		if err = randomize.Struct(seed, x, partyDBTypes, false, strmangle.SetComplement(partyPrimaryKeyColumns, partyColumnsWithoutDefault)...); err != nil {
			t.Fatal(err)
		}
	}

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = c.Insert(tx); err != nil {
		t.Fatal(err)
	}

	foreignersSplitByInsertion := [][]*Party{
		{&b, &c},
		{&d, &e},
	}

	for i, x := range foreignersSplitByInsertion {
		err = a.AddQueueParties(tx, i != 0, x...)
		if err != nil {
			t.Fatal(err)
		}

		first := x[0]
		second := x[1]

		if a.ID != first.QueueID {
			t.Error("foreign key was wrong value", a.ID, first.QueueID)
		}
		if a.ID != second.QueueID {
			t.Error("foreign key was wrong value", a.ID, second.QueueID)
		}

		if first.R.Queue != &a {
			t.Error("relationship was not added properly to the foreign slice")
		}
		if second.R.Queue != &a {
			t.Error("relationship was not added properly to the foreign slice")
		}

		if a.R.QueueParties[i*2] != first {
			t.Error("relationship struct slice not set to correct value")
		}
		if a.R.QueueParties[i*2+1] != second {
			t.Error("relationship struct slice not set to correct value")
		}

		count, err := a.QueueParties(tx).Count()
		if err != nil {
			t.Fatal(err)
		}
		if want := int64((i + 1) * 2); count != want {
			t.Error("want", want, "got", count)
		}
	}
}
func testTrackListToManyAddOpHistoryParties(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a TrackList
	var b, c, d, e Party

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, &a, trackListDBTypes, false, strmangle.SetComplement(trackListPrimaryKeyColumns, trackListColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}
	foreigners := []*Party{&b, &c, &d, &e}
	for _, x := range foreigners {
		if err = randomize.Struct(seed, x, partyDBTypes, false, strmangle.SetComplement(partyPrimaryKeyColumns, partyColumnsWithoutDefault)...); err != nil {
			t.Fatal(err)
		}
	}

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = c.Insert(tx); err != nil {
		t.Fatal(err)
	}

	foreignersSplitByInsertion := [][]*Party{
		{&b, &c},
		{&d, &e},
	}

	for i, x := range foreignersSplitByInsertion {
		err = a.AddHistoryParties(tx, i != 0, x...)
		if err != nil {
			t.Fatal(err)
		}

		first := x[0]
		second := x[1]

		if a.ID != first.HistoryID {
			t.Error("foreign key was wrong value", a.ID, first.HistoryID)
		}
		if a.ID != second.HistoryID {
			t.Error("foreign key was wrong value", a.ID, second.HistoryID)
		}

		if first.R.History != &a {
			t.Error("relationship was not added properly to the foreign slice")
		}
		if second.R.History != &a {
			t.Error("relationship was not added properly to the foreign slice")
		}

		if a.R.HistoryParties[i*2] != first {
			t.Error("relationship struct slice not set to correct value")
		}
		if a.R.HistoryParties[i*2+1] != second {
			t.Error("relationship struct slice not set to correct value")
		}

		count, err := a.HistoryParties(tx).Count()
		if err != nil {
			t.Fatal(err)
		}
		if want := int64((i + 1) * 2); count != want {
			t.Error("want", want, "got", count)
		}
	}
}

func testTrackListsReload(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = trackList.Reload(tx); err != nil {
		t.Error(err)
	}
}

func testTrackListsReloadAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := TrackListSlice{trackList}

	if err = slice.ReloadAll(tx); err != nil {
		t.Error(err)
	}
}
func testTrackListsSelect(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := TrackLists(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 1 {
		t.Error("want one record, got:", len(slice))
	}
}

var (
	trackListDBTypes = map[string]string{`CreatedAt`: `timestamp with time zone`, `Data`: `json`, `ID`: `integer`, `SpotifyPlaylistID`: `text`, `UpdatedAt`: `timestamp with time zone`}
	_                = bytes.MinRead
)

func testTrackListsUpdate(t *testing.T) {
	t.Parallel()

	if len(trackListColumns) == len(trackListPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := TrackLists(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	if err = trackList.Update(tx); err != nil {
		t.Error(err)
	}
}

func testTrackListsSliceUpdateAll(t *testing.T) {
	t.Parallel()

	if len(trackListColumns) == len(trackListPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	trackList := &TrackList{}
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := TrackLists(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	if err = randomize.Struct(seed, trackList, trackListDBTypes, true, trackListPrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	// Remove Primary keys and unique columns from what we plan to update
	var fields []string
	if strmangle.StringSliceMatch(trackListColumns, trackListPrimaryKeyColumns) {
		fields = trackListColumns
	} else {
		fields = strmangle.SetComplement(
			trackListColumns,
			trackListPrimaryKeyColumns,
		)
	}

	value := reflect.Indirect(reflect.ValueOf(trackList))
	updateMap := M{}
	for _, col := range fields {
		updateMap[col] = value.FieldByName(strmangle.TitleCase(col)).Interface()
	}

	slice := TrackListSlice{trackList}
	if err = slice.UpdateAll(tx, updateMap); err != nil {
		t.Error(err)
	}
}
func testTrackListsUpsert(t *testing.T) {
	t.Parallel()

	if len(trackListColumns) == len(trackListPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	// Attempt the INSERT side of an UPSERT
	trackList := TrackList{}
	if err = randomize.Struct(seed, &trackList, trackListDBTypes, true); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = trackList.Upsert(tx, false, nil, nil); err != nil {
		t.Errorf("Unable to upsert TrackList: %s", err)
	}

	count, err := TrackLists(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}

	// Attempt the UPDATE side of an UPSERT
	if err = randomize.Struct(seed, &trackList, trackListDBTypes, false, trackListPrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize TrackList struct: %s", err)
	}

	if err = trackList.Upsert(tx, true, nil, nil); err != nil {
		t.Errorf("Unable to upsert TrackList: %s", err)
	}

	count, err = TrackLists(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}
}
