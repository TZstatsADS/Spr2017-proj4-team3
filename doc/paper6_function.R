
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
gradient_v = function(xi, xj){ # used for updating A. Vector-based
        temp = norm_a(xi)*norm_a(xj)
        numer= xi*xj*temp - norm_a_ij2(xi, xj) * (xi^2*norm_a2(xi) + xj^2*norm_a2(xj)) / (2*temp)
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

c2_matrix <- function(txtNumber){
        X <- feature(txtNumber)
        N = nrow(X)
        c2_mtx = matrix(0, nrow = N, ncol = N)
        for (i in 1:(N-1)){
                for (j in i:N){
                        coau_i = data_list[[txtNumber]][[i]][[3]]
                        coau_j = data_list[[txtNumber]][[j]][[3]]
                        if (length(intersect(coau_i, coau_j)) > 0)
                                c2_mtx[i, j] = c2_mtx[j, i] = 1
                }
        }
        return(c2_mtx)
}

######################################################################################

Y_initial <- function(txtNumber){
        X <- feature(txtNumber)
        N = nrow(X)
        M = ncol(X)
        K = length(paper6_authorID(txtNumber))
        c2_mtx <- c2_matrix(txtNumber)
        # group initially base on c2, and compute lambda
        for (i in 1:(N-1)){
                if (intl_groups[i] != i) next
                for (j in i:N) 
                        if (c2_mtx[i, j] == 1) 
                                intl_groups[j] = i
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
                return(list(Y = Y, intl_groups = intl_groups))
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
                return(list(Y = Y, intl_groups = intl_groups))
                
                # after initialization, Y stores the clusters centers, 
        #        while intl_groups stores the initial clustering for all papers
        }

        
}

######################################################################################

paper6_EM <- function(txtNumber, has_change_assignment_in_this_iteration = TRUE){
        # 5. E-step
        X <- feature(txtNumber)
        N = nrow(X)
        M = ncol(X)
        K = length(paper6_authorID(txtNumber))
        c2_mtx <- c2_matrix(txtNumber)
        x_assign = Y_initial(txtNumber)$intl_gruops
        Y = Y_initial(txtNumber)$Y
        x_assign_in_last_e_step = rep(0, length(x_assign))
        
        while (has_change_assignment_in_this_iteration = TRUE){ # iterate between E-step & M-step
                cat("e-step\n")
                ptm = proc.time()
                #    while (TRUE){ # iteratively within E-step to update yh until equilibrium
                update_order = sample(N)
                has_change_assignment_in_this_iteration = FALSE
                ct = 0
                for (i in update_order){
                        ct = ct + 1; if (ct %% 10 == 0) cat(ct, ' ') # just print out the current status
                        min_obj = Inf
                        for (k in 1:K){ # fix all other assignment and just change the assignment for one paper
                                xi = X[i,]
                                idx_not_in_cluster_k = which(x_assign != k)
                                s1 = 0 # s1 is the same as the first term in the objective function (in Part 4.1), except that change the not-equality sign into equality sign
                                for (Ij in idx_not_in_cluster_k)
                                        if (i != Ij && c2_mtx[i,Ij]) {
                                                s1 = s1 + weighted_dist(X[i,], X[Ij,])
                                        }
                                cur_obj = weighted_dist(xi, Y[k,]) + s1 # minimize the original objective function is equivalent of minimize this objective function (i.e. cur_obj)
                                if (cur_obj < min_obj){
                                        min_obj = cur_obj
                                        new_assign = k
                                }
                        }
                        if (x_assign[i] != new_assign){
                                x_assign[i] = new_assign
                                has_change_assignment_in_this_iteration = TRUE
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
                for (k in 1:K){
                        y_temp = X[which(x_assign == k), ]
                        if (!is.vector(y_temp))
                                y_temp = apply(y_temp, 2, sum)
                        Y[k,] = y_temp / norm_a(y_temp)
                }
                # update A
                delta = rep(NA, M)
                s1 = s2 = 0
                for (i in 1:N){
                        temp_x_norm <- apply(X[which(x_assign == x_assign[i]),],2,sum)
                        s1 = s1 + gradient_v(X[i,], temp_x_norm)
                }
                for (i in 1:(N-1)) 
                        for (j in i:N) 
                                if (c2_mtx[i, j] == 1 && x_assign[i] != x_assign[j]) 
                                        s2  = s2 + gradient_v(X[i,], X[j,])
                delta = s1 + s2
                
                for (m in 1:M)
                        A[m,m] = A[m,m] + step * delta[m]
        }
        
        return(x_assign_in_last_e_step)
        
}
