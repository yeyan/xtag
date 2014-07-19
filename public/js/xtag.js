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
            redirectTo: "/index/1"
        }).
        when('/index/:page', {
            templateUrl: 'pages/bookList.html',
            controller: 'bookIndexCtrl',
            resolve: {
                response: ['$route', 'backend',
                    function($route, backend) {
                        return backend.getBookList($route.current.params.page)
                    }
                ]
            }
        }).
        when('/book/:book/:page', {
            templateUrl: 'pages/pageList.html',
            controller: 'bookCtrl',
            resolve: {
                response: ['$route', 'backend',
                    function($route, backend) {
                        return backend.getBook($route.current.params.book)
                            .success(function(data) {

                                var total = data.total;
                                var limit = 24;

                                data.total = Math.ceil(total / limit);
                                data.index = _.min([_.max([$route.current.params.page, 1]),
                                    data.total
                                ]);

                                data.items = _.range((data.index - 1) * limit + 1,
                                    _.min([data.index * limit, total]) + 1);

                                data.totalItems = total;

                                //console.log(data);

                                return data;
                            });
                    }
                ]
            }
        }).
        when('/page/:book/:page', {
            templateUrl: 'pages/page.html',
            controller: 'pageCtrl',
            resolve: {
                response: function($route, backend) {
                    return backend.getBook($route.current.params.book)
                        .success(function(data) {
                            data.index = $route.current.params.page;
                            //console.log(data);
                            return data;
                        });
                }
            }
        })
    }
]);

var toMatrix = function(data, width) {
    var matrix = [];

    (function build(data) {
        if (_.size(data) > 0) {
            matrix.push(_.take(data, width));
            build(_.drop(data, width));
        }
    })(data);

    return matrix;
}

var deltaPage = function(index, delta, total) {
    return _.max([1, _.min([parseInt(index) + delta, total])]);
}

app.controller('bookIndexCtrl', ['$scope', '$location', 'response',
    function($scope, $location, response) {
        console.log('bookIndex active');

        $scope.cols = $scope.cols || 4;

        $scope.items = toMatrix(response.data.items, $scope.cols);
        $scope.index = response.data.index;
        $scope.total = response.data.total * 10;

        $scope.preview = function(item) {
            return '/api/book/' + item.id + '/page/1/thumb';
        }

        $scope.onSelect = function(item) {
            $location.path("/book/" + item.id + "/1");
        }

        $scope.onPageChange = function() {
            $location.path("/index/" + $scope.index);
        }

        $scope.$on("navPage", function(element, delta) {
            $scope.index = deltaPage($scope.index, delta, $scope.total);
            $scope.onPageChange();
        });
    }
]);

app.controller('bookCtrl', ['$controller', '$scope', '$location', 'response',
    function($controller, $scope, $location, response) {
        console.log('pageIndex active');

        $scope.cols = 6;
        $controller('bookIndexCtrl', {
            $scope: $scope,
            $location: $location,
            response: response
        });

        $scope.name = response.data.name;
        $scope.totalItems = response.data.totalItems;

        $scope.preview = function(item) {
            return '/api/book/' + response.data.id + '/page/' + item + '/thumb';
        }

        $scope.onPageChange = function() {
            $location.path("/book/" + response.data.id + "/" + $scope.index);
        }

        $scope.onSelect = function(item) {
            $location.path("/page/" + response.data.id + "/" + item);
        }
    }
]);

app.controller('pageCtrl', ['$scope', '$location', 'response',
    function($scope, $location, response) {
        $scope.name = response.data.name;
        $scope.index = response.data.index;
        $scope.total = response.data.total;

        $scope.view = function() {
            return '/api/book/' + response.data.id + '/page/' + $scope.index + '/content';
        }

        $scope.onPageChange = function() {
            window.scrollTo(0, angular.element("[scroll-bookmark='page']").offsetTop);
        }

        $scope.$on("navPage", function(element, delta) {
            $scope.index = deltaPage($scope.index, delta, $scope.total);
            $scope.onPageChange();
        });
    }
]);

$(function() {

    function onKeyDown(e) {
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

        if (delta != 0) {
            var scope = angular.element(document).scope();

            scope.$apply(function() {
                scope.$broadcast("navPage", delta);
            });
        }
    }

    $(document).on('keydown', onKeyDown);
});

app.controller('navCtrl', function() {
    $scope.isActive = function(viewLocation) {
        return viewLocation === $location.path();
    };
});
