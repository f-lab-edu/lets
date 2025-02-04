package yeogi.moim.favorite.controller;


import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import yeogi.moim.favorite.dto.FavoriteRequest;
import yeogi.moim.favorite.dto.FavoriteResponse;
import yeogi.moim.favorite.service.FavoriteService;

import java.util.List;

@RestController
@RequestMapping("/api/favorites")
public class FavoriteController {

    private final FavoriteService favoriteService;

    public FavoriteController(FavoriteService favoriteService) {
        this.favoriteService = favoriteService;
    }

    @PostMapping
    public Long toggleFavorite(@RequestBody FavoriteRequest favoriteRequest) {
        return favoriteService.toggleFavorite(favoriteRequest);
    }

    @GetMapping
    public List<FavoriteResponse> getFavoriteList() {
        return favoriteService.getFavoriteList();
    }

}
