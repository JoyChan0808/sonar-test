package com.aam.producer.playlist.sal.client;

import com.aam.producer.playlist.sal.response.FilmHallInfo;
import java.util.List;
import java.util.Set;

public interface IFilmHallFacadeClient {

    List<FilmHallInfo> getFilmHallInfosByComplexUuids(Set<String> complexUuids);

}
