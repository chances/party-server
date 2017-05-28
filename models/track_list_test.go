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
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true); err != nil {
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
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true); err != nil {
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
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true); err != nil {
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
func trackListBeforeInsertHook(e boil.Executor, o *TrackList) error {
	*o = TrackList{}
	return nil
}

func trackListAfterInsertHook(e boil.Executor, o *TrackList) error {
	*o = TrackList{}
	return nil
}

func trackListAfterSelectHook(e boil.Executor, o *TrackList) error {
	*o = TrackList{}
	return nil
}

func trackListBeforeUpdateHook(e boil.Executor, o *TrackList) error {
	*o = TrackList{}
	return nil
}

func trackListAfterUpdateHook(e boil.Executor, o *TrackList) error {
	*o = TrackList{}
	return nil
}

func trackListBeforeDeleteHook(e boil.Executor, o *TrackList) error {
	*o = TrackList{}
	return nil
}

func trackListAfterDeleteHook(e boil.Executor, o *TrackList) error {
	*o = TrackList{}
	return nil
}

func trackListBeforeUpsertHook(e boil.Executor, o *TrackList) error {
	*o = TrackList{}
	return nil
}

func trackListAfterUpsertHook(e boil.Executor, o *TrackList) error {
	*o = TrackList{}
	return nil
}

func testTrackListsHooks(t *testing.T) {
	t.Parallel()

	var err error

	empty := &TrackList{}
	o := &TrackList{}

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, o, trackListDBTypes, false); err != nil {
		t.Errorf("Unable to randomize TrackList object: %s", err)
	}

	AddTrackListHook(boil.BeforeInsertHook, trackListBeforeInsertHook)
	if err = o.doBeforeInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeInsertHook function to empty object, but got: %#v", o)
	}
	trackListBeforeInsertHooks = []TrackListHook{}

	AddTrackListHook(boil.AfterInsertHook, trackListAfterInsertHook)
	if err = o.doAfterInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterInsertHook function to empty object, but got: %#v", o)
	}
	trackListAfterInsertHooks = []TrackListHook{}

	AddTrackListHook(boil.AfterSelectHook, trackListAfterSelectHook)
	if err = o.doAfterSelectHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterSelectHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterSelectHook function to empty object, but got: %#v", o)
	}
	trackListAfterSelectHooks = []TrackListHook{}

	AddTrackListHook(boil.BeforeUpdateHook, trackListBeforeUpdateHook)
	if err = o.doBeforeUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeUpdateHook function to empty object, but got: %#v", o)
	}
	trackListBeforeUpdateHooks = []TrackListHook{}

	AddTrackListHook(boil.AfterUpdateHook, trackListAfterUpdateHook)
	if err = o.doAfterUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterUpdateHook function to empty object, but got: %#v", o)
	}
	trackListAfterUpdateHooks = []TrackListHook{}

	AddTrackListHook(boil.BeforeDeleteHook, trackListBeforeDeleteHook)
	if err = o.doBeforeDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeDeleteHook function to empty object, but got: %#v", o)
	}
	trackListBeforeDeleteHooks = []TrackListHook{}

	AddTrackListHook(boil.AfterDeleteHook, trackListAfterDeleteHook)
	if err = o.doAfterDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterDeleteHook function to empty object, but got: %#v", o)
	}
	trackListAfterDeleteHooks = []TrackListHook{}

	AddTrackListHook(boil.BeforeUpsertHook, trackListBeforeUpsertHook)
	if err = o.doBeforeUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeUpsertHook function to empty object, but got: %#v", o)
	}
	trackListBeforeUpsertHooks = []TrackListHook{}

	AddTrackListHook(boil.AfterUpsertHook, trackListAfterUpsertHook)
	if err = o.doAfterUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterUpsertHook function to empty object, but got: %#v", o)
	}
	trackListAfterUpsertHooks = []TrackListHook{}
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
	if err = trackList.Insert(tx, trackListColumns...); err != nil {
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
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true); err != nil {
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
	if err = randomize.Struct(seed, trackList, trackListDBTypes, true); err != nil {
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