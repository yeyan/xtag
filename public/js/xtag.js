var app = angular.module('xtag', ['ngRoute', 'ui.bootstrap']);

app.factory('backend', ["$http",
    function($http) {
        return {
            getBookList: function(page) {
                return $http({
                    method: 'GET',
                    url: '/api/book/index',
                    params: {
                        page: page
                    }
                })
            },
            getBook: function(book) {
                return $http({
                    method: 'GET',
                    url: '/api/book/' + book
                })
            },
            getPage: function(book, page) {
                return $http({
                    method: 'GET',
                    url: '/api/book/' + book + '/page/' + page
                });
            },
        };
    }
]);

app.config(['$routeProvider',
    function($routeProvider) {
        $routeProvider.
        when('/', {
            redirectTo: "/list/1"
        }).
        when('/list/:page', {
            templateUrl: 'pages/bookList.html',
            controller: 'mainCtrl',
            resolve: {
                response: ['$route', 'backend',
                    function($route, backend) {
                        return backend.getBookList($route.current.params.page)
                    }
                ]
            }
        }).
        when('/page/:bookId', {
            templateUrl: 'pages/page.html',
            controller: 'pageCtrl',
            resolve: {
                book: function($route, backend) {
                    return backend.getBook($route.current.params.bookId)
                        .success(function(data) {

                            console.log(data);

                            data.id = $route.current.params.bookId;
                            data.page = 1;

                            console.log(data);
                            return data;
                        });
                }
            }
        })
    }
]);

var toMatrix = function(data, width) {
    var matrix = [];

    var index = 0;
    var currentRow = [];

    angular.forEach($scope.bookList.books, function(book) {
        index = (index + 1) % 4;
        currentRow.push(book);

        if (index == 0) {
            $scope.matrix.push(currentRow);
            currentRow = [];
        }
    });
}

app.controller('mainCtrl', ['$scope', '$location', 'response',
    function($scope, $location, response) {
        console.log("mainCtrl active");

        $scope.bookList = response.data;
        $scope.matrix = [];

        var index = 0;
        var currentRow = [];

        angular.forEach($scope.bookList.books, function(book) {
            index = (index + 1) % 4;
            currentRow.push(book);

            if (index == 0) {
                $scope.matrix.push(currentRow);
                currentRow = [];
            }
        });

        console.log($scope.matrix);

        $scope.matrix.push(currentRow);

        $scope.bookCover = function(book) {
            return '/api/book/' + book.id + '/page/1/thumb';
        }

        $scope.viewPage = function(book) {
            $location.path("/page/" + book.id);
        }

        $scope.maxPage = function() {
            return $scope.bookList.total * 10;
        }

        $scope.loadPage = function() {
            $location.path("/list/" + $scope.bookList.index);
        }
    }
]);

app.controller('pageCtrl', ['$scope', '$timeout', 'book',
    function($scope, $timeout, book) {
        console.log("pageCtrl active");
        $scope.book = book.data;

        $scope.currentPage = function() {
            return "/api/book/" + $scope.book.id + "/page/" + $scope.book.page + "/content";
        }

        $scope.maxPage = function() {
            return $scope.book.total * 10;
        }

        $scope.pageChanged = function() {
            var selector = "[scroll-bookmark='page']";
            var element = $(selector);
            if (element.length) {
                window.scrollTo(0, element[0].offsetTop - 100);
            }
        }

        function deltaPage(delta) {
            var page = $scope.book.page + delta;
            if (page < 1) page = 1;
            if (page > $scope.book.total) page = $scope.book.total;

            $scope.book.page = page;

            if (delta != 0) $scope.pageChanged();
        }


        var onKeyDown = function(e) {
            //console.log(e);

            $scope.$apply(function() {
                var delta = 0;

                switch (e.keyCode) {
                    case 39:
                        delta = e.shiftKey ? 10 : 1;
                        break;
                    case 37:
                        delta = e.shiftKey ? -10 : -1;
                        break;
                    case 32:
                        if ((window.innerHeight + window.scrollY) >= document.body.offsetHeight) {
                            delta = 1;
                            e.preventDefault();
                        }
                }

                deltaPage(delta);
            });
        }

        var onMouseClick = function(e) {
            $scope.$apply(function() {
                var middle = $(this).innerWidth() / 2;
                var delta = 1;

                if (e.offsetX < middle) {
                    delta = -1;
                }
                deltaPage(delta);
            });
        }

        $('html').on('keydown', onKeyDown);
        $('img').on('click', onMouseClick);
    }
]);

app.controller('navCtrl', function() {
    $scope.isActive = function(viewLocation) {
        return viewLocation === $location.path();
    };
});
