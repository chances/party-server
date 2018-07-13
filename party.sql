create table "user"
(
	id bigserial not null
		constraint user_pkey
			primary key,
	username varchar not null
		constraint unique_username
			unique,
	spotify_user json not null,
	spotify_playlist_id varchar,
	access_token varchar not null,
	refresh_token varchar not null,
	token_expiry_date timestamp with time zone not null,
	token_scope varchar not null,
	created_at timestamp with time zone default now() not null,
	updated_at timestamp with time zone default now() not null,
	party_id integer
)
;

create unique index user_party_id_uindex
	on "user" (party_id)
;

create table track_list
(
	id serial not null
		constraint track_list_pkey
			primary key,
	data json not null,
	created_at timestamp with time zone default now() not null,
	updated_at timestamp with time zone default now() not null,
	spotify_playlist_id text
)
;

create unique index track_list_spotify_playlist_id_uindex
	on track_list (spotify_playlist_id)
;

create table guest_list
(
	id serial not null
		constraint guest_pkey
			primary key,
	data json not null
)
;

create table party
(
	id serial not null
		constraint party_pkey
			primary key,
	location json not null,
	room_code varchar(12) not null,
	ended boolean default false not null,
	current_track json,
	queue_id integer not null
		constraint party_queue_fk
			references track_list
				on delete cascade,
	history_id integer not null
		constraint party_history_fk
			references track_list
				on delete cascade,
	guests_id integer not null
		constraint party_guests_fk
			references guest_list
				on delete cascade
)
;

create unique index party_room_code_uindex
	on party (room_code)
;

alter table "user"
	add constraint user_party_fk
		foreign key (party_id) references party
;

