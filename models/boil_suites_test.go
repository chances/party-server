// This file is generated by SQLBoiler (https://github.com/vattle/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// DO NOT EDIT

package models

import "testing"

// This test suite runs each operation test in parallel.
// Example, if your database has 3 tables, the suite will run:
// table1, table2 and table3 Delete in parallel
// table1, table2 and table3 Insert in parallel, and so forth.
// It does NOT run each operation group in parallel.
// Separating the tests thusly grants avoidance of Postgres deadlocks.
func TestParent(t *testing.T) {
	t.Run("GuestLists", testGuestLists)
	t.Run("Parties", testParties)
	t.Run("TrackLists", testTrackLists)
	t.Run("Users", testUsers)
}

func TestDelete(t *testing.T) {
	t.Run("GuestLists", testGuestListsDelete)
	t.Run("Parties", testPartiesDelete)
	t.Run("TrackLists", testTrackListsDelete)
	t.Run("Users", testUsersDelete)
}

func TestQueryDeleteAll(t *testing.T) {
	t.Run("GuestLists", testGuestListsQueryDeleteAll)
	t.Run("Parties", testPartiesQueryDeleteAll)
	t.Run("TrackLists", testTrackListsQueryDeleteAll)
	t.Run("Users", testUsersQueryDeleteAll)
}

func TestSliceDeleteAll(t *testing.T) {
	t.Run("GuestLists", testGuestListsSliceDeleteAll)
	t.Run("Parties", testPartiesSliceDeleteAll)
	t.Run("TrackLists", testTrackListsSliceDeleteAll)
	t.Run("Users", testUsersSliceDeleteAll)
}

func TestExists(t *testing.T) {
	t.Run("GuestLists", testGuestListsExists)
	t.Run("Parties", testPartiesExists)
	t.Run("TrackLists", testTrackListsExists)
	t.Run("Users", testUsersExists)
}

func TestFind(t *testing.T) {
	t.Run("GuestLists", testGuestListsFind)
	t.Run("Parties", testPartiesFind)
	t.Run("TrackLists", testTrackListsFind)
	t.Run("Users", testUsersFind)
}

func TestBind(t *testing.T) {
	t.Run("GuestLists", testGuestListsBind)
	t.Run("Parties", testPartiesBind)
	t.Run("TrackLists", testTrackListsBind)
	t.Run("Users", testUsersBind)
}

func TestOne(t *testing.T) {
	t.Run("GuestLists", testGuestListsOne)
	t.Run("Parties", testPartiesOne)
	t.Run("TrackLists", testTrackListsOne)
	t.Run("Users", testUsersOne)
}

func TestAll(t *testing.T) {
	t.Run("GuestLists", testGuestListsAll)
	t.Run("Parties", testPartiesAll)
	t.Run("TrackLists", testTrackListsAll)
	t.Run("Users", testUsersAll)
}

func TestCount(t *testing.T) {
	t.Run("GuestLists", testGuestListsCount)
	t.Run("Parties", testPartiesCount)
	t.Run("TrackLists", testTrackListsCount)
	t.Run("Users", testUsersCount)
}

func TestInsert(t *testing.T) {
	t.Run("GuestLists", testGuestListsInsert)
	t.Run("GuestLists", testGuestListsInsertWhitelist)
	t.Run("Parties", testPartiesInsert)
	t.Run("Parties", testPartiesInsertWhitelist)
	t.Run("TrackLists", testTrackListsInsert)
	t.Run("TrackLists", testTrackListsInsertWhitelist)
	t.Run("Users", testUsersInsert)
	t.Run("Users", testUsersInsertWhitelist)
}

// TestToOne tests cannot be run in parallel
// or deadlocks can occur.
func TestToOne(t *testing.T) {
	t.Run("PartyToTrackListUsingQueue", testPartyToOneTrackListUsingQueue)
	t.Run("PartyToTrackListUsingHistory", testPartyToOneTrackListUsingHistory)
	t.Run("PartyToGuestListUsingGuest", testPartyToOneGuestListUsingGuest)
	t.Run("UserToPartyUsingParty", testUserToOnePartyUsingParty)
}

// TestOneToOne tests cannot be run in parallel
// or deadlocks can occur.
func TestOneToOne(t *testing.T) {
	t.Run("PartyToUserUsingUser", testPartyOneToOneUserUsingUser)
}

// TestToMany tests cannot be run in parallel
// or deadlocks can occur.
func TestToMany(t *testing.T) {
	t.Run("GuestListToGuestParties", testGuestListToManyGuestParties)
	t.Run("TrackListToQueueParties", testTrackListToManyQueueParties)
	t.Run("TrackListToHistoryParties", testTrackListToManyHistoryParties)
}

// TestToOneSet tests cannot be run in parallel
// or deadlocks can occur.
func TestToOneSet(t *testing.T) {
	t.Run("PartyToTrackListUsingQueue", testPartyToOneSetOpTrackListUsingQueue)
	t.Run("PartyToTrackListUsingHistory", testPartyToOneSetOpTrackListUsingHistory)
	t.Run("PartyToGuestListUsingGuest", testPartyToOneSetOpGuestListUsingGuest)
	t.Run("UserToPartyUsingParty", testUserToOneSetOpPartyUsingParty)
}

// TestToOneRemove tests cannot be run in parallel
// or deadlocks can occur.
func TestToOneRemove(t *testing.T) {
	t.Run("UserToPartyUsingParty", testUserToOneRemoveOpPartyUsingParty)
}

// TestOneToOneSet tests cannot be run in parallel
// or deadlocks can occur.
func TestOneToOneSet(t *testing.T) {
	t.Run("PartyToUserUsingUser", testPartyOneToOneSetOpUserUsingUser)
}

// TestOneToOneRemove tests cannot be run in parallel
// or deadlocks can occur.
func TestOneToOneRemove(t *testing.T) {
	t.Run("PartyToUserUsingUser", testPartyOneToOneRemoveOpUserUsingUser)
}

// TestToManyAdd tests cannot be run in parallel
// or deadlocks can occur.
func TestToManyAdd(t *testing.T) {
	t.Run("GuestListToGuestParties", testGuestListToManyAddOpGuestParties)
	t.Run("TrackListToQueueParties", testTrackListToManyAddOpQueueParties)
	t.Run("TrackListToHistoryParties", testTrackListToManyAddOpHistoryParties)
}

// TestToManySet tests cannot be run in parallel
// or deadlocks can occur.
func TestToManySet(t *testing.T) {}

// TestToManyRemove tests cannot be run in parallel
// or deadlocks can occur.
func TestToManyRemove(t *testing.T) {}

func TestReload(t *testing.T) {
	t.Run("GuestLists", testGuestListsReload)
	t.Run("Parties", testPartiesReload)
	t.Run("TrackLists", testTrackListsReload)
	t.Run("Users", testUsersReload)
}

func TestReloadAll(t *testing.T) {
	t.Run("GuestLists", testGuestListsReloadAll)
	t.Run("Parties", testPartiesReloadAll)
	t.Run("TrackLists", testTrackListsReloadAll)
	t.Run("Users", testUsersReloadAll)
}

func TestSelect(t *testing.T) {
	t.Run("GuestLists", testGuestListsSelect)
	t.Run("Parties", testPartiesSelect)
	t.Run("TrackLists", testTrackListsSelect)
	t.Run("Users", testUsersSelect)
}

func TestUpdate(t *testing.T) {
	t.Run("GuestLists", testGuestListsUpdate)
	t.Run("Parties", testPartiesUpdate)
	t.Run("TrackLists", testTrackListsUpdate)
	t.Run("Users", testUsersUpdate)
}

func TestSliceUpdateAll(t *testing.T) {
	t.Run("GuestLists", testGuestListsSliceUpdateAll)
	t.Run("Parties", testPartiesSliceUpdateAll)
	t.Run("TrackLists", testTrackListsSliceUpdateAll)
	t.Run("Users", testUsersSliceUpdateAll)
}

func TestUpsert(t *testing.T) {
	t.Run("GuestLists", testGuestListsUpsert)
	t.Run("Parties", testPartiesUpsert)
	t.Run("TrackLists", testTrackListsUpsert)
	t.Run("Users", testUsersUpsert)
}
