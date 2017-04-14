# self-defined functions. For purpose of computation
norm_a = function(x){
        s = 0; for (i in 1:M) s = s + x[i]^2 * A[i,i]; return(sqrt(s))
        #   faster than (sqrt(t(x) %*% A %*% x))[1]
}
norm_a2 = function(x){
        s = 0; for (i in 1:M) s = s + x[i]^2 * A[i,i]; return(s)
}
norm_a_ij = function(xi, xj){
        s = 0; for (i in 1:M) s = s + xi[i] * xj[i] * A[i,i]; return(sqrt(s))
}
norm_a_ij2 = function(xi, xj){
        s = 0; for (i in 1:M) s = s + xi[i] * xj[i] * A[i,i]; return(s)
}
weighted_dist = function(xi, xj){
        return (1 - norm_a_ij2(xi, xj) / (norm_a(xi)*norm_a(xj)))
}
gradient_v = function(xi, xj){
        # used for updating A. Vector-based
        temp = norm_a(xi)*norm_a(xj)
        numer= xi*xj*temp - norm_a_ij2(xi, xj) * (xj^2*norm_a2(xi) + xi^2*norm_a2(xj)) / (2*temp)
        denom = temp^2
        return(numer / denom)
}
######################################################################################

paper6_authorID <- function(txtNumber){
        n=length(data_list[[txtNumber]])  #2nd name
        #coauthor=list()
        authorID=list()
        for(i in 1:n)
        {
                #coauthor[[i]]=data_list[[txtNumber]][[i]][[3]]  #coauthor list for cluster
                authorID[[i]]=data_list[[txtNumber]][[i]][[1]]  #authorID list for evaluate
        }
        # K = length(unique(as.vector(unlist(authorID))))
        return(as.vector(unlist(authorID)))
}

######################################################################################
feature <- function(txtNumber){
        
        library(text2vec)
        paper_title <- c()
        paper_id <- c()
        #paper_coauthor <- c()
        paper_journal <- list()
        for(i in 1:length(data_list[[txtNumber]])){
                paper_id[i] <- data_list[[txtNumber]][[i]][[2]]  
                paper_title[i] <- data_list[[txtNumber]][[i]][[4]]
                #paper_coauthor[i] <- paste(data_list[[2]][[i]][[3]],sep = "",collapse = "")
                #paper_coauthor[i] <- data_list[[2]][[i]][[3]]
                
                paper_journal[i] <- data_list[[txtNumber]][[i]][[5]]
        }
        
        it_train_paper <- itoken(paper_title, 
                                 preprocessor = tolower, 
                                 tokenizer = word_tokenizer,
                                 ids = paper_id,
                                 # turn off progressbar because it won't look nice in rmd
                                 progressbar = FALSE)
        vocab_paper <- create_vocabulary(it_train_paper, stopwords = c("a", "an", "the", "in", "on",
                                                                       "at", "of", "above", "under"))
        vectorizer_paper <- vocab_vectorizer(vocab_paper)
        dtm_train_paper <- create_dtm(it_train_paper, vectorizer_paper)
        tfidf_paper <- TfIdf$new()
        dtm_train_tfidf_paper <- fit_transform(dtm_train_paper, tfidf_paper)
        
        # paste(data_list[[2]][[1]][[3]],sep = "",collapse = "")
        
        it_train_journal <- itoken(paper_journal, 
                                   preprocessor = tolower, 
                                   tokenizer = word_tokenizer,
                                   ids = paper_id,
                                   # turn off progressbar because it won't look nice in rmd
                                   progressbar = FALSE)
        vocab_journal <- create_vocabulary(it_train_journal, stopwords = c("of","for","at","the","a", "an", "in", "under", "on", "above"))
        vectorizer_journal <- vocab_vectorizer(vocab_journal)
        dtm_train_journal <- create_dtm(it_train_journal, vectorizer_journal) 
        tfidf_journal <- TfIdf$new()
        dtm_train_tfidf_journal <- fit_transform(dtm_train_journal, tfidf_journal)
        dtm_train_tfidf <- cbind(dtm_train_tfidf_paper, dtm_train_tfidf_journal)
        #dtm_train_tfidf_test <- prcomp(dtm_train_tfidf)
        pca<-prcomp(dtm_train_tfidf,scale=TRUE)
        
        #find cumulative var over 95%
        sum_pca<-summary(pca)
        k<-which(sum_pca$importance[3,]>0.95)[1]
        
        dtm_train_tfidf_test <-pca$x[,1:k]
        
        return(dtm_train_tfidf_test)
}

######################################################################################
p6_compute_c2 = function(name_idx){
        N = length(data_list[[name_idx]]) # N: number of instances
        c2_mtx = diag(N)   
        for (i in 1:(N-1)){
                for (j in i:N){
                        coau_i = data_list[[name_idx]][[i]][[3]]
                        coau_j = data_list[[name_idx]][[j]][[3]]
                        if (length(intersect(coau_i, coau_j)) > 0)
                                c2_mtx[i, j] = c2_mtx[j, i] = 1
                }
        }
        return (c2_mtx)
}
######################################################################################
p6_initialization = function(c2_mtx, X, K){ 
        
        ### Input parameters: 
        # x2_mtx: c2-constraint matrix
        # X: feature matrix
        # K: number of clusters in the original files
        #
        ### return:
        # intl_groups: initial assignment of papers
        # Y: initali clusters centers
        
        N <<-  nrow(X)
        M <<-  ncol(X)
        A <<-  diag(M)
        intl_groups = 1:N
        # group initially base on c2, and compute lambda
        for (i in 1:(N-1)){
                for (j in i:N) 
                        if (c2_mtx[i, j] == 1) 
                                intl_groups[j] = intl_groups[i]
        }
        lambda = length(unique(intl_groups))
        intl_groups_idx = unique(intl_groups)
        # group into lambda clusters first
        Y = matrix(NA, max(K, lambda), M)
        for (i in 1:lambda){
                y_temp = X[intl_groups == intl_groups_idx[i],]
                if (!is.vector(y_temp))
                        y_temp = apply(y_temp, 2, sum)
                Y[i,] = y_temp / norm_a(y_temp)
        }
        # Adjust lambda into K for different cases
        if (lambda < K){
                centroid_idx = sample(N, K-lambda)
                for (i in (lambda+1):K){
                        y_temp = X[centroid_idx[i-lambda],]
                        Y[i,] = y_temp / norm_a(y_temp)
                        intl_groups[centroid_idx[i-lambda]] = i # assign xi into a new group, with only itself in the group
                }
        } else if (lambda >= K){
                # cluster initial clusters based on y value
                r = hclust(dist(Y))
                new_group_idx = cutree(r, K)
                # reassign x's
                for (i in 1:N)
                        intl_groups[i] = new_group_idx[which(intl_groups[i] == intl_groups_idx)] 
                # recompute initial y
                Y = matrix(NA, K, M)
                for (i in 1:K){
                        y_temp = X[which(intl_groups == i),]
                        if (!is.vector(y_temp))
                                y_temp = apply(y_temp, 2, sum)
                        Y[i,] = y_temp / norm_a(y_temp)
                }
        }
        return (list(intl_groups = intl_groups, Y = Y))
}
######################################################################################
p6_train_EM = function(c2_mtx, intl_groups, X, Y, K, step = 0.01){
        N <<- nrow(X)
        M <<- ncol(X)
        A = diag(M)
        x_assign = intl_groups
        x_assign_in_last_e_step = rep(0, length(x_assign))
        
        while (TRUE){ # iterate between E-step & M-step
                cat("e-step\n")
                ptm = proc.time()
                #    while (TRUE){ # iteratively within E-step to update yh until equilibrium
                update_order = sample(N)
                new_cluster = sort(unique(x_assign))
                has_change_assignment_in_this_iteration = FALSE
                ct = 0
                for (i in update_order){
                        ct = ct + 1; if (ct %% 10 == 0) cat(ct, ' ') # just print out the current status
                        min_obj = Inf
                        cur_obj <- rep(NA,K)
                        s1 <- rep(0.1,K)
                        for (k in 1:K){ # fix all other assignment and just change the assignment for one paper
                                xi = X[i,]
                                idx_not_in_cluster_k = which(x_assign != new_cluster[k])
                                #s1 = 0 # s1 is the same as the first term in the objective function (in Part 4.1), except that change the not-equality sign into equality sign
                                for (Ij in idx_not_in_cluster_k)
                                        if (i != Ij && c2_mtx[i,Ij]) {
                                                s1[k] = s1[k] + weighted_dist(X[i,], X[Ij,])
                                        }
                                cur_obj[k] = weighted_dist(xi, Y[k,]) + s1[k] # minimize the original objective function is equivalent of minimize this objective function (i.e. cur_obj)
                                # if (cur_obj < min_obj){
                                #     min_obj = cur_obj
                                # }
                        }
                        new_assign = new_cluster[which.min(cur_obj)]
                        if (x_assign[i] != new_assign){
                                has_change_assignment_in_this_iteration = TRUE
                                x_assign[i] = new_assign
                        }
                }
                #         if (!has_change_assignment_in_this_iteration) break
                #     }
                if (!has_change_assignment_in_this_iteration) break # if no assignment change from last e-step, the EM algorithm finish
                x_assign_in_last_e_step = x_assign
                proc.time() - ptm
                
                
                ###
                # 6. M-step
                
                cat("m-step\n")
                # update Y
                K = length(unique(x_assign))
                new_cluster = sort(unique(x_assign))
                
                for (k in 1:K){
                        y_temp = X[which(x_assign == new_cluster[k]), ]
                        if (!is.vector(y_temp))
                                y_temp = apply(y_temp, 2, sum)
                        Y[k,] = y_temp / norm_a(y_temp)
                }
                # update A
                delta = rep(NA, M)
                first_term <- rep(0,M)
                second_term <- rep(0,M)
                for (i in 1:N){
                        temp_x_norm <- apply(matrix(X[which(x_assign == x_assign[i]),], ncol = M),2,sum)
                        first_term = first_term + gradient_v(X[i,], temp_x_norm)
                }
                for (i in 1:(N-1)) 
                        for (j in (i+1):N) 
                                if (c2_mtx[i, j] == 1 && x_assign[i] != x_assign[j]) 
                                        second_term  = second_term + gradient_v(X[i,], X[j,])
                delta = first_term + second_term
                
                for (m in 1:M)
                        A[m,m] = A[m,m] + step * delta[m]
        }
        return (list(x_assign = x_assign, Y = Y, A = A))
}

# c2_mtx2 <- p6_compute_c2(1)
# X2 <- feature(1)
# K2 <- length(unique(paper6_authorID(1)))
# intl_groups2 <- p6_initialization(c2_mtx2, X2, K=K2)$intl_groups
# Y2 <- p6_initialization(c2_mtx2, X2, K2)$Y
# test <- p6_train_EM(c2_mtx=c2_mtx2, intl_groups=intl_groups2, X=X2, Y=Y2, K=K2)
# performance_statistics(matching_matrix(paper6_authorID(1), test$x_assign))
# performance_statistics(matching_matrix(paper6_authorID(1), intl_groups2))

